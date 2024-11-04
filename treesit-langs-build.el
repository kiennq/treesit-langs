;;; treesit-langs-build.el --- Building grammar bundle -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2021 Tuấn-Anh Nguyễn
;;
;; Author: Tuấn-Anh Nguyễn <ubolonton@gmail.com>
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; This file contains utilities to obtain and build `tree-sitter' grammars.

;; TODO: Split this into 2 libraries: one for building, which is to be used only
;; within a git checkout, and another one for downloading.

;;; Code:

(require 'seq)
(require 'pp)
(require 'url)
(require 'tar-mode)
(require 'rx)

(eval-when-compile
  (require 'subr-x)
  (require 'pcase)
  (require 'cl-lib))

(declare-function dired-omit-mode "dired-x" (&optional arg))
(declare-function magit-get-current-tag "magit-git" (&optional rev with-distance))
(declare-function magit-rev-parse "magit-git" (&rest args))

(defconst treesit-langs--dir
  (file-name-directory (locate-library "treesit-langs-build.el"))
  "The directory where the library `treesit-langs' is located.")

;; TODO: Separate build-time settings from run-time settings.
(defcustom treesit-langs-grammar-dir treesit-langs--dir
  "The root data directory of `treesit-langs'.
The `bin' directory under this directory is used to stored grammar
binaries (either downloaded, or compiled from source).

This should be set before the grammars are downloaded, e.g. before
`treesit-langs' is loaded."
  :group 'treesit-langs
  :type 'directory)

;; ---------------------------------------------------------------------------
;;; Utilities.

(defvar treesit-langs-source-alist
  `(
    ;; (markdown :url "https://github.com/tree-sitter-grammars/tree-sitter-markdown" :rev "split_parser" :src "tree-sitter-markdown/src")
    ;; (markdown-inline :url "https://github.com/tree-sitter-grammars/tree-sitter-markdown" :rev "split_parser" :src "tree-sitter-markdown-inline/src")
    )
  "List of recipes to build tree-sitter grammars.
A list with element of `(LANG . (URL REV SRC))'.
See `./script/_grammars' for a full list of languages.")

(defvar treesit-langs--out nil)

(defmacro treesit-langs--with-temp-buffer (&rest body)
  "Execute BODY with `treesit-langs--out' bound to the temporary buffer."
  (declare (indent 0))
  `(with-temp-buffer
     (let* ((treesit-langs--out (current-buffer)))
       ,@body)))

;;; TODO: Use (maybe make) an async library, with a proper event loop, instead
;;; of busy-waiting.
(defun treesit-langs--call (program &rest args)
  "Call PROGRAM with ARGS, using BUFFER as stdout+stderr.
If BUFFER is nil, `princ' is used to forward its stdout+stderr."
  (let* ((command `(,program . ,args))
         (_ (message "[treesit-langs] Running %s in %s" command default-directory))
         (base `(:name ,program :command ,command))
         (output (if treesit-langs--out
                     `(:buffer ,treesit-langs--out)
                   `(:filter (lambda (proc string)
                               (princ string)))))
         (proc (let ((process-environment (cons (format "TREE_SITTER_DIR=%s"
                                                        treesit-langs-grammar-dir)
                                                process-environment)))
                 (apply #'make-process (append base output))))
         (exit-code (progn
                      (while (not (memq (process-status proc)
                                        '(exit failed signal)))
                        (sleep-for 0.1))
                      (process-exit-status proc)))
         ;; Flush buffered output. Not doing this caused
         ;; `treesit-langs-git-dir' to be set incorrectly, and
         ;; `treesit-langs-create-bundle's output to be unordered.
         (_ (accept-process-output proc)))
    (unless (= exit-code 0)
      (error "Error calling %s, exit code is %s" command exit-code))))

(defun treesit-langs--buffer (name)
  "Return a buffer from NAME, as the DESTINATION of `call-process'.
In batch mode, return nil, so that stdout is used instead."
  (unless noninteractive
    (let ((buf (get-buffer-create name)))
      (pop-to-buffer buf)
      (delete-region (point-min) (point-max))
      (redisplay)
      buf)))

;; ---------------------------------------------------------------------------
;;; Managing language submodules.

(declare-function straight--repos-dir "straight" (&rest segments))

(defcustom treesit-langs-git-dir
  (if (featurep 'straight)
      (straight--repos-dir "treesit-langs")
    (let* ((inhibit-message t)
           (truename (file-truename (file-name-as-directory treesit-langs--dir)))
           (toplevel (ignore-errors
                       (file-truename
                        (file-name-as-directory
                         (treesit-langs--with-temp-buffer
                           (let ((default-directory treesit-langs--dir))
                             (treesit-langs--call "git" "rev-parse" "--show-toplevel"))
                           (goto-char 1)
                           (buffer-substring-no-properties 1 (line-end-position))))))))
      (if (string= truename toplevel)
          (file-name-as-directory treesit-langs--dir)
        (message "The directory %s doesn't seem to be a git working dir. Grammar-building functions will not work."
                 treesit-langs--dir)
        nil)))
  "The git working directory of the repository `treesit-langs'.
It needs to be set for grammar-building functionalities to work.

This is automatically set if you are using `straight.el', or are building from a
git checkout."
  :group 'treesit-langs
  :type 'directory)

(defun treesit-langs--repos-dir ()
  "Return the directory to store grammar repos, for compilation."
  (unless treesit-langs-git-dir
    (user-error "Grammar-building functionalities require `treesit-langs-git-dir' to be set"))
  (file-name-as-directory
   (concat treesit-langs-git-dir "repos")))

;; ---------------------------------------------------------------------------
;;; Building language grammars.
(defconst treesit-langs--bundle-version-file "BUNDLE-VERSION")

(defvar treesit-langs--bundle-version nil
  "Version of the grammar bundle.")
(defun treesit-langs--bundle-version ()
  "Version of the grammar bundle.
This should be bumped whenever a language submodule is updated, which should be
infrequent (grammar-only changes)."
  (setq treesit-langs--bundle-version
        (or treesit-langs--bundle-version
            (let ((default-directory treesit-langs-grammar-dir))
              (if (file-exists-p treesit-langs--bundle-version-file)
                  (with-temp-buffer
                    (let ((coding-system-for-read 'utf-8))
                      (insert-file-contents
                       treesit-langs--bundle-version-file)
                      (string-trim (buffer-string))))))
            "0.pre")))

(defconst treesit-langs--os
  (pcase system-type
    ('darwin "macos")
    ('gnu/linux "linux")
    ('windows-nt "windows")
    (_ (error "Unsupported system-type %s" system-type))))

(defconst treesit-langs--suffixes '(".dylib" ".dll" ".so")
  "List of suffixes for shared libraries that define tree-sitter languages.")

(defun treesit-langs--bundle-file (&optional ext version os)
  "Return the grammar bundle file's name, with optional EXT.
If VERSION and OS are not spcified, use the defaults of
`treesit-langs--bundle-version' and `treesit-langs--os'."
  (setq os (or os treesit-langs--os)
        version (or version (treesit-langs--bundle-version))
        ext (or ext ""))
  (format "tree-sitter-grammars.%s.v%s.tar%s"
          ;; FIX: Implement this correctly, refactoring 'OS' -> 'platform'.
          (pcase os
            ("windows" "x86_64-pc-windows-msvc")
            ("linux" "x86_64-unknown-linux-gnu")
            ("macos" (if (string-prefix-p "aarch64" system-configuration)
                         "aarch64-apple-darwin"
                       "x86_64-apple-darwin")))
          version ext))

;; (defvar treesit-langs--cache-dir nil "The tree-sitter parsers dir.")
;; (defun treesit-langs--cache-dir ()
;;   "The tree-sitter parsers dir."
;;   (or treesit-langs--cache-dir
;;       (setq treesit-langs--cache-dir
;;             (treesit-langs--with-temp-buffer
;;               (unless (executable-find "rsdirs")
;;                 (treesit-langs--call "cargo" "install" "--git" "https://github.com/kiennq/rust-dirs"))
;;               (expand-file-name "tree-sitter/lib"
;;                                 (treesit-langs--with-temp-buffer
;;                                   (treesit-langs--call "rsdirs" "cache")
;;                                   (goto-char 1)
;;                                   (buffer-substring-no-properties 1 (line-end-position))))))))

;; This is for compatibility with old downloading code. TODO: Remove it.
(defun treesit-langs--old-bundle-file (&optional ext version os)
  (setq os (or os treesit-langs--os)
        version (or version (treesit-langs--bundle-version))
        ext (or ext ""))
  (format "tree-sitter-grammars-%s-%s.tar%s"
          os version ext))

(cl-defun treesit-langs-compile (lang &optional target
                                      &key url rev src)
  "Download and compile the grammar for LANG-SYMBOL.
This function requires git and tree-sitter CLI."
  (message "[treesit-langs] Processing %s" lang)
  (unless (executable-find "git")
    (error "Could not find git (needed to download grammars)"))
  (unless (executable-find "tree-sitter")
    (error "Could not find tree-sitter executable (needed to compile grammars)"))
  (setq target
        (pcase (format "%s" target)
          ;; Rust's triple -> system toolchain's triple
          ("aarch64-apple-darwin" "arm64-apple-macos11")
          ("nil" nil)
          (_ (error "Unsupported cross-compilation target %s" target))))
  (message "[treesit-langs] url: %s" (alist-get lang treesit-langs-source-alist))
  (let* ((specs (alist-get lang treesit-langs-source-alist))
         (url (or url (plist-get specs :url)))
         (rev (or rev (plist-get specs :rev)))
         (src (or src (plist-get specs :src) "src"))
         (dir (file-name-as-directory
               (concat (treesit-langs--repos-dir)
                       (symbol-name lang))))
         (src-path (expand-file-name src dir))
         (bin-dir treesit-langs-grammar-dir)
         (treesit-langs--out (treesit-langs--buffer
                              (format "*treesit-langs-compile %s*" lang)))
         (cc (or (seq-find #'executable-find '("cc" "gcc" "c99" "clang"))
                 ;; If no C compiler found, just use cc and let
                 ;; `call-process' signal the error.
                 "cc"))
         (c++ (or (seq-find #'executable-find '("c++" "g++" "clang++"))
                  "c++"))
         (out-file (format "%slibtree-sitter-%s%s" bin-dir lang module-file-suffix))
         (default-directory treesit-langs-git-dir))
    (with-demoted-errors "[treesit-langs] Error: %s"
      (apply #'treesit-langs--call
             "git" "clone" url "--depth" "1" "--quiet" dir
             (when rev `("-b" ,rev))))
    (setq default-directory dir)
    (unless (file-exists-p (expand-file-name "parser.c" src-path))
      (ignore-errors (treesit-langs--call "npm" "install" "--quiet"))
      (ignore-errors (treesit-langs--call "npx" "tree-sitter" "generate")))
    ;; We need to go into the source directory because some
    ;; header files use relative path (#include "../xxx").
    ;; cd "${sourcedir}"
    (setq default-directory src-path)
    (apply #'treesit-langs--call
           (if (file-exists-p "scanner.cc") c++ cc)
           `("-shared"
             ,@(unless (memq system-type '(windows-nt cygwin)) '("-fPIC"))
             "-g" "-O2" "-I."
             ,@(when (file-exists-p "scanner.cc") '("-fno-exceptions" "-static-libstdc++" "scanner.cc" "-xc"))
             ,@(when (file-exists-p "scanner.c") '("scanner.c"))
             "parser.c"
             "-o" ,out-file
             ,@(when target `("-target" ,target))))
    ;; (when (file-exists-p (treesit-langs--cache-dir))
    ;;   (dolist (file (directory-files (treesit-langs--cache-dir) 'full ".+\\..+"))
    ;;     (unless (file-directory-p file)
    ;;       (copy-file file bin-dir :replace)
    ;;       (delete-file file))))
    ;; Replace underscores with hyphens. Example: c_sharp.
    ))

(cl-defun treesit-langs-create-bundle (&optional target version)
  "Create a bundle of language grammars.
The bundle includes all languages in `treesit-langs-source-alist'."
  (unless (executable-find "tar")
    (error "Could not find tar executable (needed to bundle compiled grammars)"))
  (let ((errors (thread-last
                  treesit-langs-source-alist
                  (mapcar
                   (pcase-lambda (`(,lang . ,specs))
                     (message "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
                     (condition-case err
                         (apply #'treesit-langs-compile lang target specs)
                       (error `[,lang ,err]))))
                  (seq-filter #'identity))))
    (message "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
    (unwind-protect
        (let* ((tar-file (concat (file-name-as-directory
                                  (expand-file-name default-directory))
                                 (treesit-langs--old-bundle-file nil version) ".gz"))
               (default-directory treesit-langs-grammar-dir)
               (treesit-langs--out (treesit-langs--buffer "*treesit-langs-create-bundle*"))
               (files (cons treesit-langs--bundle-version-file
                            (thread-last
                              (directory-files default-directory)
                              (seq-map (lambda (file)
                                            (when (seq-some (lambda (ext) (string-suffix-p ext file))
                                                            treesit-langs--suffixes)
                                              (if (string-prefix-p "libtree-sitter-" file)
                                                  file
                                                (let ((dest (concat "libtree-sitter-" file)))
                                                  (copy-file file dest :replace)
                                                  (delete-file file)
                                                  dest)))))
                              (seq-remove #'null))))
               (tar-opts nil))
          (with-temp-file treesit-langs--bundle-version-file
            (let ((coding-system-for-write 'utf-8))
              (insert (or version (treesit-langs--bundle-version)))))
          (apply #'treesit-langs--call "tar" "-zcvf" tar-file (append tar-opts files)))
      (when errors
        (message "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
        (message "[treesit-langs] Errors:\n%s" (pp-to-string errors))
        (error "Could not compile grammars!!!")))))

;; ---------------------------------------------------------------------------
;;; Download and installation.

(defconst treesit-langs--queries-dir
  (file-name-as-directory
   (concat treesit-langs--dir "queries")))

(defconst treesit-langs--repo "kiennq/treesit-langs")

(defun treesit-langs--bundle-url (&optional version os)
  "Return the URL to download the grammar bundle.
If VERSION and OS are not specified, use the defaults of
`treesit-langs--bundle-version' and `treesit-langs--os'."
  (format "https://github.com/%s/releases/download/%s/%s"
          treesit-langs--repo
          version
          (treesit-langs--bundle-file ".gz" version os)))

;;;###autoload
(defun treesit-langs-install-grammars (&optional skip-if-installed version os keep-bundle)
  "Download and install the specified VERSION of the language grammar bundle.
If VERSION or OS is not specified, use the default of
`treesit-langs--bundle-version' and `treesit-langs--os'.

This installs the grammar bundle even if the same version was already installed,
unless SKIP-IF-INSTALLED is non-nil.

When this is called interactively with a prefix argument (e.g \\[universal-argument]
\\[treesit-langs-install-grammars]) it will install the latest version instead.

The download bundle file is deleted after installation, unless KEEP-BUNDLE is
non-nil."
  (interactive (list
                nil
                (unless current-prefix-arg
                  (read-string "Bundle version: " (treesit-langs--bundle-version)))
                treesit-langs--os
                nil))
  (let* ((bin-dir treesit-langs-grammar-dir)
         (default-directory bin-dir)
         (_ (unless (file-directory-p bin-dir) (make-directory bin-dir t)))
         (has-bundle (file-exists-p
                      treesit-langs--bundle-version-file))
         (soft-forced version)
         (version (or version
                      (when (or current-prefix-arg (not has-bundle))
                        (with-current-buffer (url-retrieve-synchronously
                                          (format "https://github.com/%s/releases/"
                                                  treesit-langs--repo))
                          (goto-char (point-min))
                          (re-search-forward "\n\n" nil :noerror)
                          (when (re-search-forward (rx "/releases/tag/" (group (+ (| digit ?.))))
                                                   nil :noerror)
                            (match-string 1))))
                      (treesit-langs--bundle-version)))
         (bundle-file (treesit-langs--bundle-file ".gz" version os))
         (current-version (treesit-langs--bundle-version)))
    (cl-block nil
      (unless (or soft-forced (version< current-version version))
        (message "treesit-langs: Grammar bundle v%s was older than current one; skipped" version)
        (cl-return))
      (if (string= version current-version)
          (if skip-if-installed
              (progn (message "treesit-langs: Grammar bundle v%s was already installed; skipped" version)
                     (cl-return))
            (message "treesit-langs: Grammar bundle v%s was already installed; reinstalling" version))
        (message "treesit-langs: Installing grammar bundle v%s (was v%s)" version current-version))
      ;; FIX: Handle HTTP errors properly.
      (url-copy-file (treesit-langs--bundle-url version os)
                     bundle-file 'ok-if-already-exists)
      ;; Remove old files
      (mapc (lambda (file)
              (condition-case nil
                  (delete-file file)
                (permission-denied
                 (rename-file file (concat file ".old") :no-error))))
            (directory-files bin-dir 'full module-file-suffix))
      (treesit-langs--call "tar" "-xvzf" bundle-file)
      (unless keep-bundle
        (delete-file bundle-file))
      (setq treesit-langs--bundle-version version)
      (when (and (called-interactively-p 'any)
                 (y-or-n-p (format "Show installed grammars in %s? " bin-dir)))
        (with-current-buffer (find-file bin-dir)
          (when (bound-and-true-p dired-omit-mode)
            (dired-omit-mode -1)))))))

(provide 'treesit-langs-build)
;;; treesit-langs-build.el ends here

;;; treesit-langs.el --- TODO -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 Yoav Marco
;;
;; Author: Yoav Marco <https://github.com/ymarco>
;; Maintainer: Yoav Marco <yoavm448@gmail.com>
;; Created: May 14, 2022
;; Modified: May 14, 2022
;; Version: 0.0.1
;; Keywords: languages tools parsers tree-sitter
;; Homepage: https://github.com/emacs-tree-sitter/tree-sitter-langs
;; Package-Requires: ((emacs "29.0.50"))
;; SPDX-License-Identifier: MIT
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  TODO
;;
;;; Code:

(require 'treesit)
(require 'tree-sitter-langs-build)
(require 'treesit-faces)
(require 'rx)

(defvar treesit-langs--testing)
(eval-and-compile
  (unless (bound-and-true-p treesit-langs--testing)
    (ignore-errors
      (tree-sitter-langs-install-grammars :skip-if-installed))))

(defun treesit-langs--reformat-shared-objects (&optional lang)
  "Make symlinks so *.so files are aliased to libtree-sitter-*.so in `tree-sitter-langs--bin-dir' .

Rationale: tree-sitter-langs saves grammars as LANG.so, but
treesit needs libtree-sitter-LANG.so."
  (mapc (lambda (file)
          ;; make a symlink so that libtree-sitter-c.so points to c.so
          (let* ((name (file-name-nondirectory file))
                 (dest (concat (file-name-as-directory (file-name-directory file))
                               "libtree-sitter-" name)))
            (unless (or (string-prefix-p "libtree-sitter-" name)
                        (file-exists-p dest))
              (make-symbolic-link
               file
               dest))))
        (or (and lang `(,(concat (file-name-as-directory (tree-sitter-langs--bin-dir))
                                 (format "%s.dll" lang))))
            (directory-files (tree-sitter-langs--bin-dir) 'full
                             (rx (* any) (eval module-file-suffix) eol)))))

(defconst treesit-lang--setup-completed
  (progn
    (treesit-langs--reformat-shared-objects)
    (add-to-list 'treesit-extra-load-path (tree-sitter-langs--bin-dir))
    t))

(defun treesit-langs--convert-highlights (patterns)
  "Convert PATTERNS (a query string compatible with
elisp-tree-sitter) to a query string compatible with treesit."
  (cl-labels ((transform (exp)
                (pcase-exhaustive exp
                  ;; .match has its args flipped
                  ((or `(.match?  ,capture ,regexp)
                       `(\#match? ,capture ,regexp))
                   `(.match ,(transform regexp) ,(transform capture)))
                  ;; .equal becomes .eq
                  ((or `(.eq?  ,a ,b)
                       `(\#eq? ,a ,b))
                   `(.equal ,(transform a) ,(transform b)))
                  ;; .any-of becomes .match with regexp-opt
                  ((or `(.any-of?  ,capture . ,options)
                       `(\#any-of? ,capture . ,options))
                   `(.match ,(regexp-opt options) ,(transform capture)))
                  ;; @capture => @parent face of tree-sitter-hl-face:capture
                  ((pred symbolp)
                   (let ((name (symbol-name exp)))
                     (if (string-prefix-p "@" name)
                         (intern
                          (concat "@" "treesit-face-" (substring name 1)))
                       exp)))
                  ;; handle other cases
                  ((pred listp)
                   (mapcar #'transform exp))
                  ((pred vectorp)
                   (apply #'vector (mapcar #'transform exp)))
                  ((pred stringp)
                   exp)))
              (prin1exp
               (exp)
               (let (print-level print-length)
                 (mapconcat #'prin1-to-string exp "\n"))))
    (thread-last
      patterns
      (format "(%s)")
      ;; `read' can't handle unescaped symbols that start with "#"
      (replace-regexp-in-string "(#" "(\\\\#")
      (replace-regexp-in-string (rx (group (not (any alnum))) "?") (rx (backref 1) ":?"))
      (replace-regexp-in-string (rx (group (+ (| ? ?\())) "." (group (+ (| ? ?\) eol))))
                                (rx (backref 1) ":anchor"  (backref 2)))
      (read-from-string)
      (car)
      (transform)
      (prin1exp)
      (replace-regexp-in-string ":anchor" ".")
      (replace-regexp-in-string (rx ":?") "?")
      ;; `prin1' likes to prefix symbols that start with . with a backslash,
      ;; but the tree-sitter query parser does diffrentiate.
      (replace-regexp-in-string (regexp-quote "\\.") "."))))

(defcustom treesit-major-mode-language-alist
  '((agda-mode       . agda)
    (c++-mode        . cpp)
    (c-mode          . c)
    (caml-mode       . ocaml)
    (csharp-mode     . c-sharp)
    (css-mode        . css)
    (d-mode          . d)
    (elixir-mode     . elixir)
    (elm-mode        . elm)
    (go-mode         . go)
    (haskell-mode    . haskell)
    (hcl-mode        . hcl)
    (html-mode       . html)
    (java-mode       . java)
    (javascript-mode . javascript)
    (js-mode         . javascript)
    (js2-mode        . javascript)
    (js3-mode        . javascript)
    (json-mode       . json)
    (jsonc-mode      . json)
    (julia-mode      . julia)
    (kotlin-mode     . kotlin)
    (lua-mode        . lua)
    (mhtml-mode      . html)
    (nix-mode        . nix)
    (ocaml-mode      . ocaml)
    (php-mode        . php)
    (prisma-mode     . prisma)
    (pygn-mode       . pgn)
    (python-mode     . python)
    (rjsx-mode       . javascript)
    (ruby-mode       . ruby)
    (rust-mode       . rust)
    (rustic-mode     . rust)
    (scala-mode      . scala)
    (scheme-mode     . scheme)
    (sh-mode         . bash)
    (swift-mode      . swift)
    (terraform-mode  . hcl)
    (tuareg-mode     . ocaml)
    (typescript-mode . typescript)
    (verilog-mode    . verilog)
    (yaml-mode       . yaml)
    (zig-mode        . zig))
  "Alist that maps major modes to tree-sitter language names."
  :group 'treesit
  :type '(alist :key-type symbol
                :value-type symbol))

(defun treesit-langs--hl-query-path (lang-symbol &optional mode)
  "Return the highlighting query file for LANG-SYMBOL.
If MODE is non-nil, return the file containing additional MODE-specfic patterns
instead.  An example is `terraform-mode'-specific highlighting patterns for HCL."
  (concat (file-name-as-directory tree-sitter-langs--queries-dir)
          (file-name-as-directory (symbol-name lang-symbol))
          (if mode
              (format "highlights.%s.scm" mode)
            "highlights.scm")))

(defun treesit-langs--hl-default-patterns (lang &optional mode)
  "Return the bundled default syntax highlighting patterns for LANG and MODE.
Return nil if there are no bundled patterns."
  (with-temp-buffer
    (cl-labels ((merge-content (lang &optional mode)
                  (let ((beg (point))
                        (query-mode-file (treesit-langs--hl-query-path lang mode))
                        (query-file (treesit-langs--hl-query-path lang)))
                    (when (and mode (file-exists-p query-mode-file))
                      (insert-file-contents query-mode-file))
                    (when (file-exists-p query-file)
                      (insert-file-contents query-file))
                    (goto-char (point-max))
                    (insert "\n")
                    (goto-char beg)
                    (when (re-search-forward (rx (+ ";") (* space) "inherits" (* space) ":" (* space)
                                                 (group (* nonl) eol))
                                             nil 'noerror)
                      (dolist (dep-name (split-string (match-string 1) "," 'omit-nulls (rx (+ space))))
                        (goto-char (point-max))
                        (merge-content (intern dep-name) mode))))))
      (merge-content lang mode)
      (buffer-string))))

;;;###autoload
(defun treesit-hl-enable (&optional lang)
  "Enable `treesit-font-lock' for current buffer."
  (interactive)
  (when treesit-lang--setup-completed
    (let ((language (or lang
                        (alist-get major-mode treesit-major-mode-language-alist))))
      (treesit-langs--reformat-shared-objects language)
      (unless (and (treesit-can-enable-p)
                   (treesit-language-available-p language))
        (error "Tree sitter isn't available"))

      (setq-local treesit-font-lock-feature-list '((basic)))
      (setq-local treesit-font-lock-settings
                  (treesit-font-lock-rules
                   :language language
                   :feature 'basic
                   :override t
                   (treesit-langs--convert-highlights
                    (or (treesit-langs--hl-default-patterns language major-mode)
                        (error "No query patterns for %s" language)))))

      (unless (and (equal treesit-indent-function #'treesit-simple-indent)
                   (not (alist-get language treesit-simple-indent-rules)))
        (setq-local indent-line-function #'treesit-indent)))

    ;; better inspect
    (advice-add 'treesit-inspect-node-at-point :after
                (lambda (&rest _) (message treesit--inspect-name)))

    (setq-local font-lock-syntactic-face-function #'ignore)
    (setq-local font-lock-defaults '(nil t))
    (treesit-font-lock-enable)
    ;; Re-enable font-lock-fontify
    ;; (font-lock-mode -1)
    ;; (font-lock-mode +1)
    ))

(provide 'treesit-langs)
;;; treesit-langs.el ends here

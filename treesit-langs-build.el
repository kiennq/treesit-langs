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

(defun treesit-langs--bin-dir ()
  "Return the directory to stored grammar binaries.
This used for both compilation and downloading."
  (concat (file-name-as-directory treesit-langs-grammar-dir) "bin/"))

;; ---------------------------------------------------------------------------
;;; Utilities.

(defvar treesit-langs-source-alist
  `(
    (ada :url "https://github.com/briot/tree-sitter-ada")
    (agda :url "https://github.com/tree-sitter/tree-sitter-agda")
    (angular :url "https://github.com/dlvandenberg/tree-sitter-angular")
    (apex :url "https://github.com/aheber/tree-sitter-sfapex" :src "apex/src")
    (arduino :url "https://github.com/ObserverOfTime/tree-sitter-arduino")
    (asm :url "https://github.com/RubixDev/tree-sitter-asm")
    (astro :url "https://github.com/virchau13/tree-sitter-astro")
    (authzed :url "https://github.com/mleonidas/tree-sitter-authzed")
    (awk :url "https://github.com/Beaglefoot/tree-sitter-awk")
    (bash :url "https://github.com/tree-sitter/tree-sitter-bash")
    (bass :url "https://github.com/vito/tree-sitter-bass")
    (beancount :url "https://github.com/polarmutex/tree-sitter-beancount")
    (bibtex :url "https://github.com/latex-lsp/tree-sitter-bibtex")
    (c :url "https://github.com/tree-sitter/tree-sitter-c")
    (c-sharp :url "https://github.com/tree-sitter/tree-sitter-c-sharp")
    (clojure :url "https://github.com/sogaiu/tree-sitter-clojure")
    (cmake :url "https://github.com/uyha/tree-sitter-cmake")
    (comment :url "https://github.com/stsewd/tree-sitter-comment")
    (commonlisp :url "https://github.com/theHamsta/tree-sitter-commonlisp")
    (cpp :url "https://github.com/tree-sitter/tree-sitter-cpp")
    (css :url "https://github.com/tree-sitter/tree-sitter-css")
    (csv :url "https://github.com/amaanq/tree-sitter-csv" :src "csv/src")
    (cuda :url "https://github.com/theHamsta/tree-sitter-cuda")
    (d :url "https://github.com/gdamore/tree-sitter-d")
    (dart :url "https://github.com/UserNobody14/tree-sitter-dart")
    (devicetree :url "https://github.com/joelspadin/tree-sitter-devicetree")
    (diff :url "https://github.com/the-mikedavis/tree-sitter-diff")
    (disassembly :url "https://github.com/ColinKennedy/tree-sitter-disassembly")
    (dockerfile :url "https://github.com/camdencheek/tree-sitter-dockerfile")
    (dot :url "https://github.com/rydesun/tree-sitter-dot")
    (doxygen :url "https://github.com/amaanq/tree-sitter-doxygen")
    (dtd :url "https://github.com/tree-sitter-grammars/tree-sitter-xml" :src "dtd/src")
    (eex :url "https://github.com/connorlay/tree-sitter-eex")
    (elisp :url "https://github.com/Wilfred/tree-sitter-elisp")
    (elixir :url "https://github.com/elixir-lang/tree-sitter-elixir")
    (elm :url "https://github.com/elm-tooling/tree-sitter-elm")
    (elvish :url "https://github.com/ckafi/tree-sitter-elvish")
    (erlang :url "https://github.com/AbstractMachinesLab/tree-sitter-erlang")
    (fennel :url "https://github.com/travonted/tree-sitter-fennel")
    (fidl :url "https://github.com/google/tree-sitter-fidl")
    (fish :url "https://github.com/ram02z/tree-sitter-fish")
    (fluent :url "https://github.com/tree-sitter/tree-sitter-fluent")
    (fortran :url "https://github.com/stadelmanma/tree-sitter-fortran")
    (fsharp :url "https://github.com/ionide/tree-sitter-fsharp.git")
    (fusion :url "https://gitlab.com/jirgn/tree-sitter-fusion.git")
    (git-config :url "https://github.com/the-mikedavis/tree-sitter-git-config")
    (git-rebase :url "https://github.com/the-mikedavis/tree-sitter-git-rebase")
    (gitattributes :url "https://github.com/ObserverOfTime/tree-sitter-gitattributes")
    (gitcommit :url "https://github.com/gbprod/tree-sitter-gitcommit")
    (gitignore :url "https://github.com/shunsambongi/tree-sitter-gitignore")
    (gleam :url "https://github.com/J3RN/tree-sitter-gleam")
    (glimmer :url "https://github.com/alexlafroscia/tree-sitter-glimmer")
    (glsl :url "https://github.com/theHamsta/tree-sitter-glsl")
    (gnuplot :url "https://github.com/dpezto/tree-sitter-gnuplot")
    (go :url "https://github.com/tree-sitter/tree-sitter-go")
    (go-mod :url "https://github.com/camdencheek/tree-sitter-go-mod")
    (go-work :url "https://github.com/omertuc/tree-sitter-go-work")
    (gomod :url "https://github.com/camdencheek/tree-sitter-go-mod")
    (gotmpl :url "https://github.com/ngalaiko/tree-sitter-go-template")
    (gowork :url "https://github.com/omertuc/tree-sitter-go-work")
    (graphql :url "https://github.com/bkegley/tree-sitter-graphql")
    (groovy :url "https://github.com/murtaza64/tree-sitter-groovy")
    (haskell :url "https://github.com/tree-sitter/tree-sitter-haskell")
    (hcl :url "https://github.com/MichaHoffmann/tree-sitter-hcl")
    (heex :url "https://github.com/connorlay/tree-sitter-heex")
    (helm :url "https://github.com/ngalaiko/tree-sitter-go-template" :src "dialects/helm/src")
    (hjson :url "https://github.com/winston0410/tree-sitter-hjson")
    (hlsl :url "https://github.com/theHamsta/tree-sitter-hlsl")
    (hocon :url "https://github.com/antosha417/tree-sitter-hocon")
    (html :url "https://github.com/tree-sitter/tree-sitter-html")
    (http :url "https://github.com/rest-nvim/tree-sitter-http")
    (java :url "https://github.com/tree-sitter/tree-sitter-java")
    (javascript :url "https://github.com/tree-sitter/tree-sitter-javascript")
    (jq :url "https://github.com/flurie/tree-sitter-jq")
    (jsdoc :url "https://github.com/tree-sitter/tree-sitter-jsdoc")
    (json :url "https://github.com/tree-sitter/tree-sitter-json")
    (json5 :url "https://github.com/Joakker/tree-sitter-json5")
    (jsonnet :url "https://github.com/sourcegraph/tree-sitter-jsonnet")
    (julia :url "https://github.com/tree-sitter/tree-sitter-julia")
    (kotlin :url "https://github.com/fwcd/tree-sitter-kotlin")
    (kusto :url "https://github.com/Willem-J-an/tree-sitter-kusto")
    (lalrpop :url "https://github.com/traxys/tree-sitter-lalrpop")
    (latex :url "https://github.com/latex-lsp/tree-sitter-latex" )
    (ledger :url "https://github.com/cbarrete/tree-sitter-ledger")
    (llvm :url "https://github.com/benwilliamgraham/tree-sitter-llvm")
    (lua :url "https://github.com/MunifTanjim/tree-sitter-lua")
    (m68k :url "https://github.com/grahambates/tree-sitter-m68k")
    (make :url "https://github.com/alemuller/tree-sitter-make")
    (markdown :url "https://github.com/tree-sitter-grammars/tree-sitter-markdown" :rev "split_parser" :src "tree-sitter-markdown/src")
    (markdown-inline :url "https://github.com/tree-sitter-grammars/tree-sitter-markdown" :rev "split_parser" :src "tree-sitter-markdown-inline/src")
    (matlab :url "https://github.com/acristoffers/tree-sitter-matlab")
    (mediawiki :url "https://github.com/Ordoviz/tree-sitter-mediawiki")
    (menhir :url "https://github.com/Kerl13/tree-sitter-menhir")
    (mermaid :url "https://github.com/monaqa/tree-sitter-mermaid")
    (meson :url "https://github.com/Decodetalkers/tree-sitter-meson")
    (nasm :url "https://github.com/naclsn/tree-sitter-nasm")
    (nim :url "https://github.com/alaviss/tree-sitter-nim")
    (ninja :url "https://github.com/alemuller/tree-sitter-ninja")
    (nix :url "https://github.com/cstrahan/tree-sitter-nix")
    (norg :url "https://github.com/nvim-neorg/tree-sitter-norg")
    (objc :url "https://github.com/amaanq/tree-sitter-objc")
    (objdump :url "https://github.com/ColinKennedy/tree-sitter-objdump")
    (ocaml :url "https://github.com/tree-sitter/tree-sitter-ocaml" :src "grammars/ocaml/src")
    (ocaml-interface :url "https://github.com/tree-sitter/tree-sitter-ocaml" :src "grammars/interface/src")
    (ocaml-type :url "https://github.com/tree-sitter/tree-sitter-ocaml" :src "grammars/type/src")
    (ocamllex :url "https://github.com/atom-ocaml/tree-sitter-ocamllex")
    (pascal :url "https://github.com/Isopod/tree-sitter-pascal")
    (perl :url "https://github.com/tree-sitter-perl/tree-sitter-perl" :rev "release")
    (pgn :url "https://github.com/rolandwalker/tree-sitter-pgn.git")
    (php :url "https://github.com/tree-sitter/tree-sitter-php" :src "php/src")
    (pioasm :url "https://github.com/leo60228/tree-sitter-pioasm")
    (powershell :url "https://github.com/airbus-cert/tree-sitter-powershell")
    (prisma :url "https://github.com/victorhqc/tree-sitter-prisma")
    (proto :url "https://github.com/treywood/tree-sitter-proto")
    (psv :url "https://github.com/amaanq/tree-sitter-csv" :src "psv/src")
    (python :url "https://github.com/tree-sitter/tree-sitter-python")
    (ql :url "https://github.com/tree-sitter/tree-sitter-ql")
    (qmljs :url "https://github.com/yuja/tree-sitter-qmljs")
    (query :url "https://github.com/nvim-treesitter/tree-sitter-query")
    (r :url "https://github.com/r-lib/tree-sitter-r.git")
    (racket :url "https://github.com/6cdh/tree-sitter-racket")
    (rasi :url "https://github.com/Fymyte/tree-sitter-rasi")
    (regex :url "https://github.com/tree-sitter/tree-sitter-regex")
    (rego :url "https://github.com/FallenAngel97/tree-sitter-rego")
    (rnoweb :url "https://github.com/bamonroe/tree-sitter-rnoweb")
    (rst :url "https://github.com/stsewd/tree-sitter-rst")
    (ruby :url "https://github.com/tree-sitter/tree-sitter-ruby")
    (rust :url "https://github.com/tree-sitter/tree-sitter-rust")
    (scala :url "https://github.com/tree-sitter/tree-sitter-scala")
    (scheme :url "https://github.com/6cdh/tree-sitter-scheme.git")
    (scss :url "https://github.com/serenadeai/tree-sitter-scss")
    (solidity :url "https://github.com/JoranHonig/tree-sitter-solidity")
    (sparql :url "https://github.com/BonaBeavis/tree-sitter-sparql")
    (sql :url "https://github.com/derekstride/tree-sitter-sql" :rev "gh-pages")
    (supercollider :url "https://github.com/madskjeldgaard/tree-sitter-supercollider")
    (surface :url "https://github.com/connorlay/tree-sitter-surface")
    (svelte :url "https://github.com/Himujjal/tree-sitter-svelte")
    (swift :url "https://github.com/alex-pinkus/tree-sitter-swift" )
    (sxhkdrc :url "https://github.com/RaafatTurki/tree-sitter-sxhkdrc")
    (teal :url "https://github.com/euclidianAce/tree-sitter-teal" )
    (tiger :url "https://github.com/ambroisie/tree-sitter-tiger")
    (tlaplus :url "https://github.com/tlaplus-community/tree-sitter-tlaplus")
    (tmux :url "https://github.com/Freed-Wu/tree-sitter-tmux")
    (toml :url "https://github.com/ikatyang/tree-sitter-toml")
    (tsv :url "https://github.com/amaanq/tree-sitter-csv" :src "tsv/src")
    (tsx :url "https://github.com/tree-sitter/tree-sitter-typescript" :src "tsx/src")
    (turtle :url "https://github.com/BonaBeavis/tree-sitter-turtle")
    (twig :url "https://github.com/gbprod/tree-sitter-twig")
    (typescript :url "https://github.com/tree-sitter/tree-sitter-typescript" :src "typescript/src")
    (typst :url "https://github.com/uben0/tree-sitter-typst")
    (ungrammar :url "https://github.com/tree-sitter-grammars/tree-sitter-ungrammar")
    (vala :url "https://github.com/vala-lang/tree-sitter-vala")
    (verilog :url "https://github.com/tree-sitter/tree-sitter-verilog.git")
    (vhs :url "https://github.com/charmbracelet/tree-sitter-vhs")
    (vim :url "https://github.com/neovim/tree-sitter-vim")
    (vimdoc :url "https://github.com/neovim/tree-sitter-vimdoc")
    (vls :url "https://github.com/vlang/vls" :src "tree_sitter_v/src")
    (vue :url "https://github.com/ikatyang/tree-sitter-vue")
    (wgsl :url "https://github.com/szebniok/tree-sitter-wgsl")
    (xml :url "https://github.com/tree-sitter-grammars/tree-sitter-xml" :src "xml/src")
    (yaml :url "https://github.com/ikatyang/tree-sitter-yaml")
    (yang :url "https://github.com/Hubro/tree-sitter-yang")
    (zig :url "https://github.com/maxxnino/tree-sitter-zig")
    )
  "List of recipes to build tree-sitter grammars.
A list with element of `(LANG . (URL REV SRC))'")

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
            (let ((default-directory (treesit-langs--bin-dir)))
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
         (bin-dir (treesit-langs--bin-dir))
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
               (default-directory (treesit-langs--bin-dir))
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
  (let* ((bin-dir (treesit-langs--bin-dir))
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

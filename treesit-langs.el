;;; treesit-langs.el --- TODO -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 Yoav Marco
;;
;; Author: Yoav Marco <https://github.com/ymarco>
;; Maintainer: Yoav Marco <yoavm448@gmail.com>, Kien Nguyen
;; Created: May 14, 2022
;; Modified: May 14, 2022
;; Version: 1.0
;; Keywords: languages tools parsers tree-sitter
;; Package-Requires: ((emacs "29.1"))
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
(require 'treesit-langs-build)
(require 'treesit-faces)
(require 'font-lock)
(require 'rx)

;; (defvar treesit-langs--testing)
;; (unless (bound-and-true-p treesit-langs--testing)
;;   (ignore-errors
;;     (treesit-langs-install-grammars :skip-if-installed)))


(defun treesit-langs--reformat-shared-objects (&optional lang)
  "Make symlinks so parsers are aliased in `treesit-langs-grammar-dir'.

Rationale: treesit-langs saves grammars as LANG.so, but
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
        (or (and lang `(,(concat (file-name-as-directory treesit-langs-grammar-dir)
                                 (format "%s.dll" lang))))
            (directory-files treesit-langs-grammar-dir 'full
                             (rx (* any) (eval `(or ,@treesit-langs--suffixes)) eol)))))

(defvar treesit-lang--setup-completed nil)

(defun treesit-lang--setup ()
  "Setup parsers."
  (treesit-langs--reformat-shared-objects)
  (add-to-list 'treesit-extra-load-path treesit-langs-grammar-dir)
  (setq treesit-lang--setup-completed t))

(defun treesit-langs--convert-highlights (patterns)
  "Convert PATTERNS to a query string compatible with treesit.
PATTERNS is a query string compatible with `elisp-tree-sitter'."
  (cl-labels ((transform (exp)
                (pcase-exhaustive exp
                  ;; .match has its args flipped
                  ((or `(.match?  ,capture ,regexp)
                       `(\#match? ,capture ,regexp)
                       `(.lua-match? ,capture ,regexp)
                       `(\#lua-match? ,capture ,regexp))
                   `(:match ,(transform regexp) ,(transform capture)))
                  ;; .equal becomes .eq
                  ((or `(.eq?  ,a ,b)
                       `(\#eq? ,a ,b))
                   `(:equal ,(transform a) ,(transform b)))
                  ;; .any-of becomes .match with regexp-opt
                  ((or `(.any-of?  ,capture . ,options)
                       `(\#any-of? ,capture . ,options))
                   `(:match ,(regexp-opt options) ,(transform capture)))
                  ('\? `:?)
                  ('+ `:+)
                  ('* `:*)
                  ('@spell '_)
                  ('@nospell '_)
                  ;; @capture => @parent face of tree-sitter-hl-face:capture
                  ((pred symbolp)
                   (let ((name (symbol-name exp)))
                     (cond
                      ((and (string-prefix-p "@" name)
                            (not (string-prefix-p "@_" name)))
                       (intern
                        (concat "@" "treesit-face-" (substring name 1))))
                      (:default exp))))
                  ;; handle other cases
                  (`(,op . ,_)
                   (if (and (symbolp op)
                            (string-prefix-p "#" (symbol-name op))
                            (not (memq op '(\#match? \#lua-match? \#eq? \#any-of?))))
                       '_
                     (mapcar #'transform exp)))
                  ((pred vectorp)
                   (apply #'vector (mapcar #'transform exp)))
                  ((pred stringp)
                   (replace-regexp-in-string (rx "\\?") "?" exp))
                  ((pred numberp) exp)
                  ))
              (prin1exp
                (exp)
                (let (print-level print-length)
                  (mapconcat #'prin1-to-string exp "\n"))))
    (thread-last
      patterns
      (format "(%s)")
      ;; `read' can't handle unescaped symbols that start with "#"
      (replace-regexp-in-string "(#" "(\\\\#")
      ;; See https://www.gnu.org/software/emacs/manual/html_node/elisp/Pattern-Matching.html
      (replace-regexp-in-string (rx (group (not (any alnum))) "?") (rx (backref 1) "\?"))
      (replace-regexp-in-string (rx (group (+ (| ? ?\())) "." (group (+ (| ? ?\) eol))))
                                (rx (backref 1) ":anchor"  (backref 2)))
      (read-from-string)
      (car)
      (transform)
      ;; (prin1exp)
      ;; (replace-regexp-in-string ":anchor" ".")
      ;; (replace-regexp-in-string (rx ":" (group (| "equal" "match" "pred"))) (rx "." (backref 1)))
      ;; (replace-regexp-in-string (rx ":" (group (| "?" "*" "+"))) (rx (backref 1)))
      ;; ;; `prin1' likes to prefix symbols that start with . with a backslash,
      ;; ;; but the tree-sitter query parser does diffrentiate.
      ;; (replace-regexp-in-string (regexp-quote "\\.") ".")
      )))

(defcustom treesit-major-mode-language-alist
  '(
    (adoc-mode            . (asciidoc asciidoc-inline))
    (agda-mode            . agda)
    (asm-mode             . asm)
    (bash-ts-mode         . bash)
    (c++-mode             . cpp)
    (c++-ts-mode          . cpp)
    (c-mode               . c)
    (c-ts-mode            . c)
    (caml-mode            . ocaml)
    (clojure-mode         . clojure)
    (cmake-mode           . cmake)
    (cmake-ts-mode        . cmake)
    (cperl-mode           . perl)
    (csharp-mode          . c-sharp)
    (csharp-ts-mode       . c-sharp)
    (css-mode             . css)
    (css-ts-mode          . css)
    (csv-mode             . csv)
    (d-mode               . d)
    (diff-mode            . diff)
    (dockerfile-mode      . dockerfile)
    (dockerfile-ts-mode   . dockerfile)
    (elixir-mode          . elixir)
    (elm-mode             . elm)
    (emacs-lisp-mode      . elisp)
    (erlang-mode          . erlang)
    (ess-r-mode           . r)
    (fennel-mode          . fennel)
    (fsharp-mode          . fsharp)
    (fsharp-ts-mode       . fsharp)
    (gdb-disassembly-mode . disassembly)
    (git-commit-mode      . gitcommit)
    (git-rebase-mode      . git-rebase)
    (gitattributes-mode   . gitattributes)
    (gitconfig-mode       . git-config)
    (gitignore-mode       . gitignore)
    (go-mod-ts-mode       . gomod)
    (go-mode              . go)
    (go-ts-mode           . go)
    (graphql-mode         . graphql)
    (graphql-ts-mode      . graphql)
    (graphviz-dot-mode    . dot)
    (groovy-mode          . groovy)
    (haskell-mode         . haskell)
    (hcl-mode             . hcl)
    (html-mode            . (html javascript css jsdoc))
    (java-mode            . java)
    (java-ts-mode         . java)
    (javascript-mode      . javascript)
    (js-mode              . javascript)
    (js-ts-mode           . javascript)
    (js2-mode             . javascript)
    (js3-mode             . javascript)
    (json-mode            . json)
    (json-ts-mode         . json)
    (jsonc-mode           . json)
    (julia-mode           . julia)
    (kotlin-mode          . kotlin)
    (kusto-mode           . kusto)
    (latex-mode           . latex)
    (lua-mode             . lua)
    (makefile-mode        . make)
    (markdown-ts-mode     . (markdown markdown-inline))
    (matlab-mode          . matlab)
    (mermaid-mode         . mermaid)
    (mermaid-ts-mode      . mermaid)
    (meson-mode           . meson)
    (ninja-mode           . ninja)
    (nix-mode             . nix)
    (nxml-mode            . xml)
    (objc-mode            . objc)
    (ocaml-mode           . ocaml)
    (pascal-mode          . pascal)
    (perl-mode            . perl)
    (php-mode             . (php phpdoc html javascript jsdoc css))
    (powershell-mode      . powershell)
    (powershell-ts-mode   . powershell)
    (prisma-mode          . prisma)
    (psv-mode             . psv)
    (pygn-mode            . pgn)
    (python-mode          . python)
    (python-ts-mode       . python)
    (rjsx-mode            . javascript)
    (ruby-mode            . ruby)
    (rust-mode            . rust)
    (rust-ts-mode         . rust)
    (rustic-mode          . rust)
    (scala-mode           . scala)
    (scheme-mode          . scheme)
    (sgml-mode            . dtd)
    (sh-mode              . bash)
    (sql-mode             . sql)
    (sql-ts-mode          . sql)
    (swift-mode           . swift)
    (terraform-mode       . hcl)
    (toml-ts-mode         . toml)
    (tsv-mode             . tsv)
    (tsx-ts-mode          . tsx)
    (tuareg-mode          . ocaml)
    (typescript-mode      . typescript)
    (typescript-ts-mode   . typescript)
    (typst-ts-mode        . typst)
    (verilog-mode         . verilog)
    (vimrc-mode           . vim)
    (xml-mode             . xml)
    (yaml-mode            . yaml)
    (yaml-ts-mode         . yaml)
    (zig-mode             . zig)
    (zig-ts-mode          . zig)
    )
  "Alist that maps major modes to tree-sitter language names."
  :group 'treesit
  :type '(alist :key-type symbol
                :value-type symbol))

(defun treesit-langs--hl-query-path (lang-symbol &optional mode)
  "Return the highlighting query file for LANG-SYMBOL.
If MODE is non-nil, return the file containing additional MODE-specfic patterns
instead.  An example is `terraform-mode'-specific highlighting patterns for HCL."
  (concat (file-name-as-directory treesit-langs--queries-dir)
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
                      (insert-file-contents query-file)
                      (goto-char (point-max))
                      (insert "\n"))
                    (goto-char beg)
                    (when (re-search-forward (rx (+ ";") (* space) "inherits" (* space) ":" (* space)
                                                 (group (* nonl) eol))
                                             nil 'noerror)
                      (dolist (dep-name (split-string (match-string 1) "," 'omit-nulls (rx (+ space))))
                        (goto-char (point-max))
                        (merge-content (intern dep-name) mode))))))
      (merge-content lang mode)
      (let ((content (buffer-string)))
        (unless (string-empty-p content) content)))))

(defvar-local treesit-hl--enabled nil "Non-nil if the treesit highlighting should be used.")
(put 'treesit-hl--enabled 'permanent-local t)

(defun treesit-hl--on (&optional langs)
  "Turn on tree-sitter highlighting for current buffer with language LANGS.
LANGS can be a list or a symbol."
  (unless treesit-lang--setup-completed
    (treesit-lang--setup))
  (when-let* ((languages (or langs
                             (let* ((modes `(,major-mode))
                                    (mode (pop modes))
                                    l)
                               (while (and mode (not (setq l (alist-get mode treesit-major-mode-language-alist))))
                                 (mapc (lambda (p-mode)
                                         (add-to-list 'modes p-mode))
                                       `(,@(get mode 'derived-mode-extra-parents) ,(get mode 'derived-mode-parent)))
                                 (setq mode (pop modes)))
                               l)))
              (languages (pcase languages
                           ((pred listp) languages)
                           (`,val (list val))))
              (font-lock-settings t))
    (dolist (language languages)
      (unless (treesit-ready-p language)
        (error "Tree sitter for %s isn't available" language))
      (treesit-parser-create language))

    (setq font-lock-settings
          (apply #'treesit-font-lock-rules
                 (mapcan (lambda (lang)
                           (list :language lang
                                 :feature 'override
                                 :override t
                                 (treesit-langs--convert-highlights
                                  (or (treesit-langs--hl-default-patterns lang major-mode)
                                      (error "No query patterns for %s" lang)))))
                         languages)))

    ;; NOTE: the alternative approach is to reinvoke the `major-mode' inside
    ;; `delay-mode-hooks' to make sure the `treesit-font-lock' is set up
    ;; correctly. We will need to run `delayed-mode-hooks' afterward. However,
    ;; this approach is not efficient.
    (setq-local treesit-font-lock-settings font-lock-settings)
    (setq-local treesit-font-lock-feature-list '((override)))
    (let (treesit-simple-indent-rules)
      (treesit-major-mode-setup))
    (setq font-lock-major-mode nil)
    (font-lock-update)
    (mapc #'kill-local-variable
          '(whitespace-mode-set-explicitly
            whitespace-mode-major-mode))
    (run-hooks 'after-change-major-mode-hook)
    (message "Turn on tree-sitter.")))

(defun treesit-hl--off ()
  "Turn off tree-sitter highlighting for current buffer."
  (funcall-interactively major-mode)
  (message "Turn off tree-sitter."))

;;;###autoload
(defun treesit-hl-toggle (&optional enable)
  "Toggle tree-sitter highlighting state according to ENABLE."
  (interactive (list (not treesit-hl--enabled)))
  (setq treesit-hl--enabled enable)
  (cl-letf (((symbol-function 'treesit-hl-toggle) #'ignore))
    (if treesit-hl--enabled
        (treesit-hl--on)
      (treesit-hl--off))))


(provide 'treesit-langs)
;;; treesit-langs.el ends here

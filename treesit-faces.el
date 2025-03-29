;;; treesit-langs.el --- TODO -*- lexical-binding: t; -*-
;;
;; Version: 0.0.1
;; Keywords: languages tools parsers tree-sitter
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

(require 'font-lock)

(defgroup treesit-faces nil
  "Faces for highlighting code."
  :group 'treesit)

;;; ------------------------------------
;;; Bases.
(defface treesit-face-base
  '((t :foreground unspecified :inherit nil))
  "Base face for treesit.")

;;; ------------------------------------
;;; Functions.

(defface treesit-face-function
  '((default :inherit font-lock-function-name-face))
  "Face for function declarations, definitions and bindings."
  :group 'treesit-faces)

(defface treesit-face-function.call
  '((default :inherit (link font-lock-function-name-face) :underline nil))
  "Face for function calls."
  :group 'treesit-faces)

(defface treesit-face-function.builtin
  '((default :inherit treesit-face-function))
  "Face for builtin functions."
  :group 'treesit-faces)

;;; TODO: Remove this?
(defface treesit-face-function.special
  '((default :inherit font-lock-preprocessor-face))
  "Face for functions that alter things at compile/load time."
  :group 'treesit-faces)

;;; TODO: Rename this?
(defface treesit-face-function.macro
  '((default :inherit font-lock-preprocessor-face))
  "Face for macro calls."
  :group 'treesit-faces)

(defface treesit-face-method
  '((default :inherit treesit-face-function))
  "Face for method declarations and definitions."
  :group 'treesit-faces)

(defface treesit-face-method.call
  '((default :inherit treesit-face-function.call))
  "Face for method invocations."
  :group 'treesit-faces)

;;; ------------------------------------
;;; Types.

(defface treesit-face-type
  '((default :inherit font-lock-type-face))
  "Face for types."
  :group 'treesit-faces)

(defface treesit-face-type.parameter
  '((default :inherit font-lock-variable-name-face))
  "Face for type parameters."
  :group 'treesit-faces)

(defface treesit-face-type.argument
  '((default :inherit treesit-face-type))
  "Face for type arguments."
  :group 'treesit-faces)

(defface treesit-face-type.builtin
  '((default :inherit treesit-face-type))
  "Face for builtin types."
  :group 'treesit-faces)

(defface treesit-face-type.super
  '((default :inherit treesit-face-type))
  "Face for super types in definitions and type constraints."
  :group 'treesit-faces)

;;; TODO: Remove this?
(defface treesit-face-constructor
  '((default :inherit treesit-face-type))
  "Face for constructors."
  :group 'treesit-faces)

;;; ------------------------------------
;;; Variables, properties.

;;; TODO: Add variable.use?
(defface treesit-face-variable
  '((default :inherit font-lock-variable-name-face))
  "Face for variable declarations, definitions, bindings and mutations."
  :group 'treesit-faces)

(defface treesit-face-variable.parameter
  '((default :inherit treesit-face-variable))
  "Face for function parameters."
  :group 'treesit-faces)

(defface treesit-face-variable.builtin
  '((default :inherit treesit-face-variable))
  "Face for builtin variables."
  :group 'treesit-faces)

(defface treesit-face-variable.special
  '((default :inherit font-lock-warning-face))
  "Face for \"dangerous\" variables, e.g. mutable or dynamically-bound."
  :group 'treesit-faces)

(defface treesit-face-property
  '((default :inherit font-lock-variable-name-face :slant italic))
  "Face for properties."
  :group 'treesit-faces)

(defface treesit-face-property.definition
  '((default :inherit treesit-face-variable.parameter))
  "Face for property declarations and definitions."
  :group 'treesit-faces)

;;; ------------------------------------
;;; Strings, comments, text proses.

(defface treesit-face-comment
  '((default :inherit font-lock-comment-face))
  "Face for comments."
  :group 'treesit-faces)

(defface treesit-face-comment.documentation
  '((default :inherit font-lock-doc-face))
  "Face for docstrings."
  :group 'treesit-faces)

(defface treesit-face-string
  '((default :inherit font-lock-string-face))
  "Face for strings."
  :group 'treesit-faces)

(defface treesit-face-string.special
  '((default :inherit treesit-face-string :weight bold))
  "Face for special strings, e.g. regular expressions."
  :group 'treesit-faces)

(defface treesit-face-string.emphasis
  '((default :inherit treesit-face-string :slant italic))
  "Face for emphasised strings."
  :group 'treesit-faces)

(defface treesit-face-string.strike
  '((default :inherit treesit-face-string :strike-through t))
  "Face for striked strings."
  :group 'treesit-faces)

(defface treesit-face-string.underline
  '((default :inherit treesit-face-string :underline t))
  "Face for underlined strings."
  :group 'treesit-faces)

(defface treesit-face-string.uri
  '((default :inherit link))
  "Face for uri."
  :group 'treesit-faces)

(defface treesit-face-escape
  '((default :inherit font-lock-keyword-face))
  "Face for escape characters in strings."
  :group 'treesit-faces)

;;; TODO: Rename this?
(defface treesit-face-embedded
  '((default :inherit default))
  "Face for embedded expressions and code fragments."
  :group 'treesit-faces)

;;; ------------------------------------
;;; Atomics, constants.

(defface treesit-face-keyword
  '((default :inherit font-lock-keyword-face))
  "Face for keywords."
  :group 'treesit-faces)

(defface treesit-face-operator
  '((default :inherit treesit-face-keyword))
  "Face for operators."
  :group 'treesit-faces)

(defface treesit-face-label
  '((default :inherit font-lock-preprocessor-face))
  "Face for labels."
  :group 'treesit-faces)

(defface treesit-face-constant
  '((default :inherit font-lock-constant-face))
  "Face for constants."
  :group 'treesit-faces)

(defface treesit-face-constant.builtin
  '((default :inherit treesit-face-constant))
  "Face for builtin constants."
  :group 'treesit-faces)

(defface treesit-face-number
  '((default :inherit treesit-face-constant))
  "Face for numbers."
  :group 'treesit-faces)

(defface treesit-face-module
  '((default :inherit font-lock-constant-face))
  "Face for constants."
  :group 'treesit-faces)

;;; ------------------------------------
;;; Punctuations (aka. should-be-dimmed).

(defface treesit-face-punctuation
  '((default :inherit default))
  "Face for punctuations."
  :group 'treesit-faces)

(defface treesit-face-punctuation.bracket
  '((default :inherit treesit-face-punctuation))
  "Face for brackets."
  :group 'treesit-faces)

(defface treesit-face-punctuation.delimiter
  '((default :inherit treesit-face-punctuation))
  "Face for delimiters."
  :group 'treesit-faces)

(defface treesit-face-punctuation.special
  '((default :inherit treesit-face-keyword))
  "Face for special punctuations."
  :group 'treesit-faces)

;;; ------------------------------------
;;; Markups.

(defface treesit-face-tag
  '((default :inherit font-lock-function-name-face :weight bold))
  "Face for tags in markup languages."
  :group 'treesit-faces)

(defface treesit-face-attribute
  '((default :inherit font-lock-preprocessor-face))
  "Face for attributes markup languages."
  :group 'treesit-faces)

(defface treesit-face-markup.heading
  '((t (:inherit treesit-face-base)))
  "Face for headings in markup languages.")
(defface treesit-face-markup.heading.base
  `((t (:inherit font-lock-function-name-face
        :weight bold)))
  "Base face for headers.")

(defface treesit-face-markup.heading.1
  `((t (:inherit (font-lock-keyword-face treesit-face-markup.heading.base))))
  "Face for level 1 headings in markup languages.")

(defface treesit-face-markup.heading.2
  `((t (:inherit (font-lock-variable-name-face treesit-face-markup.heading.base))))
  "Face for level 2 headings in markup languages.")

(defface treesit-face-markup.heading.3
  `((t (:inherit (font-lock-function-name-face treesit-face-markup.heading.base))))
  "Face for level 3 headings in markup languages.")

(defface treesit-face-markup.heading.4
  `((t (:inherit (font-lock-comment-face treesit-face-markup.heading.base))))
  "Face for level 4 headings in markup languages.")

(defface treesit-face-markup.heading.5
  `((t (:inherit (font-lock-type-face treesit-face-markup.heading.base))))
  "Face for level 5 headings in markup languages.")

(defface treesit-face-markup.heading.6
  `((t (:inherit (font-lock-constant-face treesit-face-markup.heading.base))))
  "Face for level 6 headings in markup languages.")

(defface treesit-face-markup.list
  '((t (:inherit shadow :slant normal :weight normal)))
  "Face for list item markers.")

(defface treesit-face-markup.raw
  '((t (:inherit (treesit-face-base font-lock-constant-face))))
  "Face for inline code.")

(defface treesit-face-markup.strikethrough
  '((t (:strike-through t)))
  "Face for strikethrough text.")

(put 'treesit-face-markup.italic 'face-alias 'link)
(put 'treesit-face-markup.link 'face-alias 'link)
(put 'treesit-face-markup.link.label 'face-alias 'link)
(put 'treesit-face-markup.link.url 'face-alias 'font-lock-string-face)
(put 'treesit-face-markup.list.checked 'face-alias 'font-lock-builtin-face)
(put 'treesit-face-markup.list.unchecked 'face-alias 'font-lock-builtin-face)
(put 'treesit-face-markup.strong 'face-alias 'bold)

;;; ------------------------------------
;;; Errors.
(defface treesit-face-error
  '((default :inherit error))
  "Face for attributes markup languages."
  :group 'treesit-faces)

;;; ------------------------------------
;;; Aliases
(put 'treesit-face-boolean 'face-alias 'treesit-face-constant.builtin)
(put 'treesit-face-character 'face-alias 'treesit-face-variable)
(put 'treesit-face-character.special 'face-alias 'treesit-face-variable)
(put 'treesit-face-class 'face-alias 'treesit-face-type)
(put 'treesit-face-comment.warning 'face-alias 'treesit-face-comment)
(put 'treesit-face-conditional 'face-alias 'treesit-face-keyword)
(put 'treesit-face-conditional.ternary 'face-alias 'treesit-face-keyword)
(put 'treesit-face-constant.macro 'face-alias 'treesit-face-function.macro)
(put 'treesit-face-define 'face-alias 'treesit-face-function.macro)
(put 'treesit-face-exception 'face-alias 'treesit-face-keyword)
(put 'treesit-face-field 'face-alias 'treesit-face-variable)
(put 'treesit-face-function.method 'face-alias 'treesit-face-method)
(put 'treesit-face-function.method.call 'face-alias 'treesit-face-method.call)
(put 'treesit-face-include 'face-alias 'treesit-face-function.macro)
(put 'treesit-face-keyword.conditional 'face-alias 'treesit-face-keyword)
(put 'treesit-face-keyword.coroutine 'face-alias 'treesit-face-keyword)
(put 'treesit-face-keyword.directive 'face-alias 'treesit-face-function.macro)
(put 'treesit-face-keyword.directive.define 'face-alias 'treesit-face-define)
(put 'treesit-face-keyword.exception 'face-alias 'treesit-face-keyword)
(put 'treesit-face-keyword.function 'face-alias 'treesit-face-function)
(put 'treesit-face-keyword.import 'face-alias 'treesit-face-function.macro)
(put 'treesit-face-keyword.modifier 'face-alias 'treesit-face-keyword)
(put 'treesit-face-keyword.operator 'face-alias 'treesit-face-operator)
(put 'treesit-face-keyword.repeat 'face-alias 'treesit-face-keyword)
(put 'treesit-face-keyword.return 'face-alias 'treesit-face-keyword)
(put 'treesit-face-keyword.storage 'face-alias 'treesit-face-keyword)
(put 'treesit-face-keyword.type 'face-alias 'treesit-face-keyword)
(put 'treesit-face-library 'face-alias 'treesit-face-function.macro)
(put 'treesit-face-namespace 'face-alias 'treesit-face-keyword)
(put 'treesit-face-none 'face-alias 'default)
(put 'treesit-face-number.float 'face-alias 'treesit-face-number)
(put 'treesit-face-parameter 'face-alias 'treesit-face-type.parameter)
(put 'treesit-face-preproc 'face-alias 'treesit-face-function.macro)
(put 'treesit-face-repeat 'face-alias 'treesit-face-keyword)
(put 'treesit-face-spell 'face-alias 'treesit-face-comment)
(put 'treesit-face-storageclass 'face-alias 'treesit-face-type)
(put 'treesit-face-storageclass.lifetime 'face-alias 'treesit-face-type.argument)
(put 'treesit-face-string.escape 'face-alias 'treesit-face-escape)
(put 'treesit-face-string.regex 'face-alias 'treesit-face-string)
(put 'treesit-face-string.special.path 'face-alias 'treesit-face-string)
(put 'treesit-face-string.special.symbol 'face-alias 'treesit-face-variable)
(put 'treesit-face-string.special.url 'face-alias 'treesit-face-string.uri)
(put 'treesit-face-tag.attribute 'face-alias 'treesit-face-attribute)
(put 'treesit-face-tag.delimiter 'face-alias 'treesit-face-punctuation.bracket)
(put 'treesit-face-text 'face-alias 'treesit-face-string)
(put 'treesit-face-text.emphasis 'face-alias 'treesit-face-string.emphasis)
(put 'treesit-face-text.literal 'face-alias 'treesit-face-string)
(put 'treesit-face-text.strike 'face-alias 'treesit-face-string.strike)
(put 'treesit-face-text.strong 'face-alias 'treesit-face-string.special)
(put 'treesit-face-text.title 'face-alias 'treesit-face-string.special)
(put 'treesit-face-text.underline 'face-alias 'treesit-face-string.underline)
(put 'treesit-face-text.uri 'face-alias 'treesit-face-string.uri)
(put 'treesit-face-type.definition 'face-alias 'treesit-face-property.definition)
(put 'treesit-face-type.qualifier 'face-alias 'treesit-face-keyword)
(put 'treesit-face-variable.member 'face-alias 'treesit-face-parameter)

;; Diff
(put 'treesit-face-text.diff.add 'face-alias 'diff-added)
(put 'treesit-face-text.diff.delete 'face-alias 'diff-removed)

(provide 'treesit-faces)
;;; treesit-faces.el ends here

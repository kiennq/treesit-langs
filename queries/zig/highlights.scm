[
  (container_doc_comment)
  (doc_comment)
  (line_comment)
] @comment @spell

((line_comment) @text.note
  (#match? @text.note "^// *zig fmt: (on|off) *$")
)

[
  variable: (IDENTIFIER)
  variable_type_function: (IDENTIFIER)
] @variable

parameter: (IDENTIFIER) @parameter

[
  field_member: (IDENTIFIER)
  field_access: (IDENTIFIER)
] @field

;; assume TitleCase is a type
(
  [
    variable_type_function: (IDENTIFIER)
    field_access: (IDENTIFIER)
    parameter: (IDENTIFIER)
  ] @type
  (#match? @type "^[A-Z]")
)
;; assume camelCase is a function
(
  [
    variable_type_function: (IDENTIFIER)
    field_access: (IDENTIFIER)
    parameter: (IDENTIFIER)
  ] @function
  (#match? @function "^[a-z]+[A-Z]+")
)

;; assume all CAPS_1 is a constant
(
  [
    variable_type_function: (IDENTIFIER)
    field_access: (IDENTIFIER)
  ] @constant
  (#match? @constant "^[A-Z][A-Z_0-9]+$")
)

[
  function_call: (IDENTIFIER)
  function: (IDENTIFIER)
] @function.call

exception: "!" @exception

(
  (IDENTIFIER) @variable.builtin
  (#eq? @variable.builtin "_")
)

; (PtrTypeStart "c" @variable.builtin)

; (
;   (ContainerDeclType
;       (ErrorUnionExpr)
;       ; "enum"
;   )
;   (ContainerField (IDENTIFIER) @constant)
; )

field_constant: (IDENTIFIER) @constant

(BUILTINIDENTIFIER) @function.builtin

((BUILTINIDENTIFIER) @include
  (#any-of? @include "@import" "@cImport"))

(INTEGER) @number

(NUMBER.FLOAT) @number.float

[
  (LINESTRING)
  (STRINGLITERALSINGLE)
] @string @spell

(CHAR_LITERAL) @character
(EscapeSequence) @string.escape
(FormatSequence) @string.special

[
  "allowzero"
  "volatile"
  "threadlocal"
  "inline"
  "noinline"
  "noalias"
] @type.qualifier

[
  "anytype"
  "anyframe"
  (BuildinTypeExpr)
] @type.builtin

(BreakLabel (IDENTIFIER) @label)
(BlockLabel (IDENTIFIER) @label)

[
  "true"
  "false"
] @boolean

[
  "undefined"
  "unreachable"
  "null"
] @constant.builtin

[
  "else"
  "if"
  "switch"
] @conditional

[
  "for"
  "while"
] @repeat

[
  "or"
  "and"
  "orelse"
] @keyword.operator

[
  "packed"
  "opaque"
  "comptime"
] @storageclass

[
  "struct"
  "enum"
  "union"
  "error"
  "defer"
  "errdefer"
  "async"
  "nosuspend"
  "await"
  "suspend"
  "resume"
  "export"
  "extern"
  "asm"
  "callconv"
] @keyword

[
  "try"
  "error"
  "catch"
] @exception

; VarDecl
[
  "const"
  "var"
  "fn"
] @keyword.function

[
  "test"
  "pub"
  "usingnamespace"
] @keyword

[
  "return"
  "break"
  "continue"
] @keyword.return

[
  "linksection"
  "align"
] @function.builtin

[
  (CompareOp)
  (BitwiseOp)
  (BitShiftOp)
  (AdditionOp)
  (AssignOp)
  (MultiplyOp)
  (PrefixOp)
  "*"
  "**"
  "->"
  "=>"
  ".?"
  ".*"
  "?"
] @operator

[
  ";"
  "."
  ","
  ":"
] @punctuation.delimiter

[
  ".."
  "..."
] @punctuation.special

[
  "["
  "]"
  "("
  ")"
  "{"
  "}"
  (Payload "|")
  (PtrPayload "|")
  (PtrIndexPayload "|")
] @punctuation.bracket

; Error
(ERROR) @error

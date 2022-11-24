; inherits: c

((identifier) @field
 (#match? @field "(^_|^m_|_$)"))

(parameter_declaration
  declarator: (reference_declarator) @parameter)
; function(Foo ...foo)
(variadic_parameter_declaration
  declarator: (variadic_declarator
                (_) @parameter))
; int foo = 0
(optional_parameter_declaration
    declarator: (_) @parameter)

;(field_expression) @parameter ;; How to highlight this?
(template_function
  name: (identifier) @function)

(template_method
  name: (field_identifier) @method)

(field_initializer
 (field_identifier) @property)

(function_declarator
  declarator: (field_identifier) @method)

(concept_definition
  name: (identifier) @type)

(namespace_identifier) @namespace
((namespace_identifier) @type
                        (#match? @type "^[A-Z]"))
((namespace_identifier) @constant
                        (#match? @constant "^[A-Z][A-Z_0-9]*$"))
(case_statement
  value: (qualified_identifier (identifier) @constant))
(namespace_definition
  name: (identifier) @namespace)

(using_declaration . "using" . "namespace" . [(qualified_identifier) (identifier)] @namespace)

(destructor_name
  (identifier) @method)

(function_declarator
      declarator: (qualified_identifier
        name: (identifier) @function))
(function_declarator
      declarator: (qualified_identifier
        name: (qualified_identifier
          name: (identifier) @function)))
((function_declarator
      declarator: (qualified_identifier
        name: (identifier) @constructor))
 (#match? @constructor "^[A-Z]"))

(operator_name) @function
"operator" @function
"static_assert" @function.builtin

(call_expression
  function: (qualified_identifier
              name: (identifier) @function.call))
(call_expression
  function: (qualified_identifier
              name: (qualified_identifier
                      name: (identifier) @function.call)))
(call_expression
  function:
      (qualified_identifier
        name: (qualified_identifier
              name: (qualified_identifier
                      name: (identifier) @function.call))))

(call_expression
  function: (field_expression
              field: (field_identifier) @function.call))

((call_expression
  function: (identifier) @constructor)
(#match? @constructor "^[A-Z]"))
((call_expression
  function: (qualified_identifier
              name: (identifier) @constructor))
(#match? @constructor "^[A-Z]"))

((call_expression
  function: (field_expression
              field: (field_identifier) @constructor))
(#match? @constructor "^[A-Z]"))

;; constructing a type in an initializer list: Constructor ():  **SuperType (1)**
((field_initializer
  (field_identifier) @constructor
  (argument_list))
 (#match? @constructor "^[A-Z]"))


; Constants

(this) @variable.builtin
(nullptr) @constant

(true) @boolean
(false) @boolean

; Literals

(raw_string_literal)  @string

; Keywords

[
 "try"
 "catch"
 "noexcept"
 "throw"
] @exception


[
 "class"
 "co_await"
 "concept"
 "consteval"
 "constexpr"
 "constinit"
 "decltype"
 "explicit"
 "final"
 "friend"
 "mutable"
 "namespace"
 "override"
 "private"
 "protected"
 "public"
 "requires"
 "requires"
 "template"
 "typename"
 "using"
 "virtual"
 (auto)
] @keyword

[
 "co_yield"
 "co_return"
] @keyword.return

[
 "new"
 "delete"

 ;; these keywords are not supported by the parser
 ;"eq"
 ;"not_eq"
 ;
 ;"compl"
 ;"and"
 ;"or"
 ;
 ;"bitand"
 ;"bitand_eq"
 ;"bitor"
 ;"bitor_eq"
 ;"xor"
 ;"xor_eq"
] @keyword.operator

"<=>" @operator

"::" @punctuation.delimiter

(literal_suffix) @operator

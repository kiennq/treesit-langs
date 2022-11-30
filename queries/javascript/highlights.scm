; inherits: ecma,jsx

(optional_chain) @punctuation.delimiter

;;; Parameters
(formal_parameters (identifier) @variable.parameter)

(formal_parameters
  (rest_pattern
    (identifier) @variable.parameter))

;; ({ a }) => null
(formal_parameters
  (object_pattern
    (shorthand_property_identifier_pattern) @variable.parameter))

;; ({ a: b }) => null
(formal_parameters
  (object_pattern
    (pair_pattern
      value: (identifier) @variable.parameter)))

;; ([ a ]) => null
(formal_parameters
  (array_pattern
    (identifier) @variable.parameter))

;; Function and method definitions

(function
 name: (identifier) @function)
(function_declaration
 name: (identifier) @function)
(method_definition
 name: (property_identifier) @method)

(variable_declarator
 name: (identifier) @function
 value: [(function) (arrow_function)])

(assignment_expression
 left: [(identifier) @function
        (member_expression property: (property_identifier) @method)]
 right: [(function) (arrow_function)])

(pair key: (property_identifier) @method
      value: [(function) (arrow_function)])

;; Function and method calls

(call_expression
 function: [(identifier) @function.call
            (member_expression
             property: (property_identifier) @method.call)])

;; Variables

(variable_declarator
 name: (identifier) @variable)
(assignment_expression
 left: [(identifier) @variable
        (member_expression property: (property_identifier) @variable)])
(augmented_assignment_expression
 left: [(identifier) @variable
        (member_expression property: (property_identifier) @variable)])
(for_in_statement
 left: (identifier) @variable)

;; a => null
(arrow_function
  parameter: (identifier) @variable.parameter)

;; optional parameters
(formal_parameters
  (assignment_pattern
    left: (identifier) @variable.parameter))

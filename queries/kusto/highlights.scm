; Comments and literals
(comment) @comment
(string_literal) @string
(command_string) @string
(integer_literal) @number
(long_literal_expression) @number
(int_literal_expression) @number
(real_number) @number
(real_literal_expression) @number
(decimal_literal_expression) @number
(time_span_literal_expression) @number
(date_time_literal_expression) @string.special
(boolean_literal_expression) @boolean
(json_null) @constant.builtin
(guid_literal_expression) @constant
(raw_guid_literal_expression) @constant
(raw_guid) @constant
(type_literal_expression) @type
(scalar_type) @type

; Management commands
(management_command name: (command_word) @keyword.directive)
(count_operator "count" @keyword)
(get_schema_operator) @keyword
(execute_and_cache_operator) @keyword

; Declarations, calls, parameters, entities, properties, and names
(command_name) @variable
(let_function_declaration
  name: (identifier_or_keyword_or_escaped_name) @function)
(let_variable_declaration
  name: (identifier_or_keyword_or_escaped_name) @variable)
(named_function_call_expression
  name: (simple_name_reference
    (identifier_or_keyword_or_escaped_name) @function.call))
(count_expression "count" @function.call)
(parameter_name) @variable.parameter
(entity_name_reference) @module
(before_pipe_expression
  (unnamed_expression
    (logical_or_expression
      (logical_and_expression
        (equality_expression
          (relational_expression
            (additive_expression
              (multiplicative_expression
                (string_operator_expression
                  (string_binary_operator_expression
                    (invocation_expression
                      (function_call_or_path_expression
                        (function_call_or_path_root
                          (primary_expression
                            (name_reference_with_data_scope
                              (simple_name_reference
                                (identifier_or_keyword_or_escaped_name) @module)))))))))))))))
  (#match? @module "^[A-Za-z_][A-Za-z0-9_]*$"))
(named_expression_name_clause
  (identifier_or_extended_keyword_or_escaped_name) @property)
(function_call_or_path_operation
  (identifier_or_keyword_or_escaped_name) @property)
(query_operator_property (identifier) @property)
(render_operator (identifier) @property)
(project_expression
  (simple_or_wildcarded_name_reference
    (simple_name_reference
      (identifier_or_keyword_or_escaped_name) @property)))

; Query keywords and operators
[
  "alias"
  "database"
  "declare"
  "pattern"
  "query_parameters"
  "let"
  "view"
  "materialize"
  "set"
  "restrict"
  "access"
  "print"
  "macro-expand"
  "range"
  "datatable"
  "externaldata"
  "external_data"
  "materialized-view-combine"
  "to"
  "from"
  "where"
  "filter"
  "as"
  "assert-schema"
  "consume"
  "distinct"
  "extend"
  "facet"
  "by"
  "find"
  "fork"
  "graph-mark-components"
  "graph-match"
  "graph-shortest-paths"
  "graph-to-table"
  "nodes"
  "edges"
  "invoke"
  "join"
  "lookup"
  "make-graph"
  "make-series"
  "mv-expand"
  "mv-apply"
  "evaluate"
  "parse"
  "parse-kv"
  "parse-where"
  "partition"
  "project"
  "project-away"
  "project-keep"
  "project-rename"
  "project-reorder"
  "reduce"
  "render"
  "sample"
  "sample-distinct"
  "scan"
  "search"
  "serialize"
  "sort"
  "summarize"
  "take"
  "top"
  "top-hitters"
  "top-nested"
  "union"
  "execute"
  "on"
  "in"
  "kind"
  "with"
  "optional"
  "output"
  "order"
  "asc"
  "desc"
  "of"
  "step"
] @keyword

[
  "|"
  "="
  "=="
  "!="
  "<"
  ">"
  "<="
  ">="
  "=~"
  "!~"
  "has"
  "!has"
  "has_cs"
  "!has_cs"
  "hasprefix"
  "!hasprefix"
  "hassuffix"
  "!hassuffix"
  "like"
  "likecs"
  "contains"
  "containscs"
  "!contains"
  "!contains_cs"
  "startswith"
  "startswith_cs"
  "endswith"
  "endswith_cs"
  "matches regex"
  "+"
  "-"
  "*"
  "/"
  "%"
  "and"
  "or"
] @operator

; Brackets and delimiters
[
  "("
  ")"
  "["
  "]"
  "{"
  "}"
] @punctuation.bracket

[
  ","
  ";"
  ":"
] @punctuation.delimiter

(command_bracket) @punctuation.bracket
(command_punct) @punctuation.delimiter

; Ambiguous names must be last so contextual captures above win.
(simple_name_reference) @variable

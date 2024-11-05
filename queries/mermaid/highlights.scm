; adapted from https://github.com/monaqa/tree-sitter-mermaid
[
  "sequenceDiagram"
  "classDiagram"
  "classDiagram-v2"
  "stateDiagram"
  "stateDiagram-v2"
  "gantt"
  "pie"
  "flowchart"
  "erdiagram"
  "participant"
  "as"
  "activate"
  "deactivate"
  "note "
  "over"
  "link"
  "links"
  ; "left of"
  ; "right of"
  "properties"
  "details"
  "title"
  "loop"
  "rect"
  "opt"
  "alt"
  "else"
  "par"
  "and"
  "end"
  (sequence_stmt_autonumber)
  (note_placement_left)
  (note_placement_right)
  "class"
  "state "
  "dateformat"
  "inclusiveenddates"
  "topaxis"
  "axisformat"
  "includes"
  "excludes"
  "todaymarker"
  "title"
  "section"
  "direction"
  "subgraph"
] @keyword

(comment) @comment @spell

[
  ":"
  (sequence_signal_plus_sign)
  (sequence_signal_minus_sign)
  (class_visibility_public)
  (class_visibility_private)
  (class_visibility_protected)
  (class_visibility_internal)
  (state_division)
] @punctuation.delimiter

[
  "("
  ")"
  "{"
  "}"
] @punctuation.bracket

[
  "-->"
  (solid_arrow)
  (dotted_arrow)
  (solid_open_arrow)
  (dotted_open_arrow)
  (solid_cross)
  (dotted_cross)
  (solid_point)
  (dotted_point)
] @operator

[
  (class_reltype_extension)
  (class_reltype_composition)
  (class_reltype_dependency)
  (class_linetype_solid)
  (class_linetype_dotted)
  "&"
] @operator

(sequence_actor) @type

(class_name) @type

(state_name) @type

[
  (class_annotation_line)
  (class_stmt_annotation)
  (class_generics)
  (state_annotation_fork)
  (state_annotation_join)
  (state_annotation_choice)
] @attribute

(directive) @keyword.import

[
  (flow_text_quoted)
  (flow_text_literal)
  (flow_arrow_text)
  (flow_vertex_text)
  (sequence_text)
  (sequence_alias)
  (state_description)
  (er_role)
  (cardinality)
  (gantt_task_text)
  (pie_title)
  (pie_label)
  (pie_label)
] @string

(pie_value) @number.float

[
  (direction_lr)
  (direction_rl)
  (direction_tb)
  (direction_bt)
  (flowchart_direction_lr)
  (flowchart_direction_rl)
  (flowchart_direction_tb)
  (flowchart_direction_bt)
  (annotation)
] @constant

(flow_vertex_id) @type

[
  (flow_link_arrow)
  (flow_link_arrow_start)
  (sequence_signal_type)
  (state_arrow)
  (er_relation)
  (class_relation)
] @operator

(flow_link_arrowtext
  "|" @punctuation.bracket)

(flow_vertex_square
  [
    "["
    "]"
  ] @punctuation.bracket)

(flow_vertex_circle
  [
    "(("
    "))"
  ] @punctuation.bracket)

(flow_vertex_ellipse
  [
    "(-"
    "-)"
  ] @punctuation.bracket)

(flow_vertex_stadium
  [
    "(["
    "])"
  ] @punctuation.bracket)

(flow_vertex_subroutine
  [
    "[["
    "]]"
  ] @punctuation.bracket)

(flow_vertex_rect
  [
    "[|"
    "|]"
  ] @punctuation.bracket)

(flow_vertex_cylinder
  [
    "[("
    ")]"
  ] @punctuation.bracket)

(flow_vertex_round
  [
    "("
    ")"
  ] @punctuation.bracket)

(flow_vertex_diamond
  [
    "{"
    "}"
  ] @punctuation.bracket)

(flow_vertex_hexagon
  [
    "{{"
    "}}"
  ] @punctuation.bracket)

(flow_vertex_odd
  [
    ">"
    "]"
  ] @punctuation.bracket)

(flow_vertex_trapezoid
  [
    "[/"
    "\\]"
  ] @punctuation.bracket)

(flow_vertex_inv_trapezoid
  [
    "[\\"
    "/]"
  ] @punctuation.bracket)

(flow_vertex_leanright
  [
    "[/"
    "/]"
  ] @punctuation.bracket)

(flow_vertex_leanleft
  [
    "[\\"
    "\\]"
  ] @punctuation.bracket)

(flow_stmt_subgraph
  [
    "["
    "]"
  ] @punctuation.bracket)

[
  (er_cardinarity_zero_or_one)
  (er_cardinarity_zero_or_more)
  (er_cardinarity_one_or_more)
  (er_cardinarity_only_one)
  (er_reltype_non_identifying)
  (er_reltype_identifying)
] @operator

(er_entity_name) @type

(er_attribute_type) @constant
(er_attribute_name) @variable.member

[
  (er_attribute_key_type_pk)
  (er_attribute_key_type_fk)
] @keyword.modifier

(er_attribute_comment) @string @spell

(gantt_section) @type
(gantt_task_data) @type
(gantt_date_format) @constant
(gantt_axis_format) @constant

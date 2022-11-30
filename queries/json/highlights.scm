[(true) (false)] @boolean
[(null)] @constant.builtin
(number) @number

(pair key: (string) @keyword)
(pair value: (string) @string)

(array (string) @string)
(string_content (escape_sequence) @string.escape)

["," ":"] @punctuation.delimiter
["{" "}" "[" "]"] @punctuation.bracket

(escape_sequence) @escape
(comment) @comment

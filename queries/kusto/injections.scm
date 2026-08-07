((script_query_prefix) @injection.content
  (#set! injection.language "kusto"))

((command_brace_body) @injection.content
  (#set! injection.language "kusto")
  (#offset! @injection.content 0 1 0 -1))

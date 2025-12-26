[
  (while_statement)
  (if_statement)
  (for_statement)
  (function_definition)
  (struct_declaration)
  (union_declaration)
  (enum_declaration)
] @local.scope

; Definitions

(assignment_expression assignee: (expression (ident) @local.definition)

(function_definition name: (ident) @local.definition)

(parameter_list (variable_signature (ident) @local.definition))

[ (ident) ] @local.reference

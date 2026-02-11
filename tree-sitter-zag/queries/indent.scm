[
  (block)
  (if_statement)
  (if_expression)
  (while_statement)
  (for_statement)
  (struct_declaration)
  (enum_declaration)
  (union_declaration)
  (match_expression)

  (struct_instantiation_expression)
  (array_instantiation_expression)

  (parameter_list)
  (argument_list)
  (generic_parameter_list)
  (generic_argument_list)
] @indent

[
  "("
  ")"
  "["
  "]"
  "{"
  "}"
  "<"
  ">"
] @indent.branch

[
  (comment)
] @ignore

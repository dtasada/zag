"return" @keyword.return

[
 "as"
 "bind"
 "break"
 "continue"
 "else"
 "enum"
 "fn"
 "for"
 "if"
 "import"
 "let"
 "match"
 "mut"
 "pub"
 "struct"
 "union"
 "while"
] @keyword

(while_statement "while" @repeat)
(if_statement ["if" "else"] @conditional)
(for_statement "for" @repeat)
(function_definition "fn" @keyword.function)

(binary_expression op: _ @operator)
(prefix_expression op: _ @operator)
(assignment_operator) @operator
[".." "..=" "&" "!" "?" "="] @operator

[";" "." "," ":" "..." "->"] @punctuation.delimiter

[
 "(" ")"
 "[" "]"
 "{" "}"
 "<" ">"
 "|"
] @punctuation.bracket

(ident) @variable

((ident) @constant (#match? @constant "^[A-Z][A-Z_0-9]*$"))

[
 (true_literal)
 (false_literal)
 (null_literal)
 (undefined_literal)
] @constant.builtin

(member_expression rhs: (ident) @field)

(parameter_list (variable_signature name: (ident) @variable.parameter))
(parameter_list name: (ident) @variable.parameter)

(function_definition name: (ident) @function)
(binding_function_declaration name: (ident) @function)

(call_expression callee: (expression (ident) @function.call))
; (call_expression callee: (expression (member_expression (rhs: (ident) @function.call))))
(argument_list argument: (expression (ident) @variable.parameter))
(capture (ident) @variable.parameter)

(comment) @comment
(string_literal) @string
(number_literal) @number
(char_literal) @number
(ident_type) @type
(primitive_type) @type.builtin

(variable_definition variable_name: (ident) @variable)
(struct_instantiation_expression_member member_name: (ident) @field)

(struct_member name: (ident) @field)
(enum_member name: (ident) @field)
(union_member name: (ident) @field)

((ident) @variable.builtin
  (#eq? @variable.builtin "_"))

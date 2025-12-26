; (function_definition name: (ident) @definition.function)
; (struct_declaration name: (ident_type) @name) @definition.class
; (enum_declaration name: (ident_type) @name) @definition.class
; (union_declaration name: (ident_type) @name) @definition.class
;
; ((ident_type) @reference (#set! reference.kind "type"))
;
; (variable_definition
;   variable_name: (ident) @definition.var
; )
;
; (ident) @reference
;
; ; Scopes
; [
;   (block)
;   (if_expression)
;   (while_expression)
;   (for_expression)
;   (test_expression)
; ] @scope

;;;;;;;; lua example
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

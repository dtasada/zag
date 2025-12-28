/**
  * @file Zag grammar for tree-sitter
  * @author Dani Tasada <daniteeem@gmail.com>
  * @license MIT
  */

/// <reference types="tree-sitter-cli/dsl" />
// @ts-check

function commaSep(item) {
  return seq(repeat(seq(item, ",")), optional(item));
}

export default grammar({
  name: "zag",

  extras: $ => [
    /\s+/,
    $.comment,
  ],

  rules: {
    source_file: $ => repeat($._statement),

    _statement: $ => choice(
      $.return_statement,
      $.variable_definition,
      $.struct_declaration,
      $.enum_declaration,
      $.union_declaration,
      $.function_definition,
      $.if_statement,
      $.while_statement,
      $.for_statement,
      $.expression_statement,
    ),

    expression_statement: $ => seq($.expression, ";"),

    expression: $ => choice(
      $.ident,
      $.string_literal,
      $.char_literal,
      $.number_literal,

      $.true_literal,
      $.false_literal,
      $.null_literal,
      $.undefined_literal,

      $.call_expression,
      $.member_expression,
      $.binary_expression,
      $.prefix_expression,
      $.assignment_expression,
      $.struct_instantiation_expression,
      $.array_instantiation_expression,
      $.block,
      $.if_expression,
      $.range_expression,
      $.reference_expression,
    ),

    function_definition: $ => seq(
      "fn",
      field("name", $.ident),
      field("parameters", $.parameter_list),
      field("return_type", $.type),
      field("body", $.block),
    ),

    variable_definition: $ => seq(
      "let",
      optional("mut"),
      field("variable_name", $.ident),
      optional(seq(
        ":",
        field("type", $.type),
      )),
      "=",
      field("assigned_value", $.expression),
      ";",
    ),

    struct_declaration: $ => seq(
      "struct",
      field("name", $.ident_type),
      optional(field("generics", $.parameter_list)),
      "{",
      // repeat(choice(
        commaSep(field("member", $.struct_member)),
        repeat(field("method", $.function_definition)),
      // )),
      "}",
    ),

    struct_member: $ => seq(
      field("name", $.ident),
      ":",
      field("type", $.type),
      optional(seq(
        "=",
        field("default_value", $.expression),
      )),
    ),

    enum_declaration: $ => seq(
      "enum",
      optional(field("type", $.parameter_list)),
      field("name", $.ident_type),
      "{",
      // repeat(choice(
        commaSep(field("member", $.enum_member)),
        repeat(field("method", $.function_definition)),
      // )),
      "}",
    ),

    enum_member: $ => seq(
      field("name", $.ident),
      optional(seq(
        "=",
        field("value", $.expression),
      )),
    ),

    union_declaration: $ => seq(
      "union",
      field("name", $.ident_type),
      "{",
      // repeat(choice(
        commaSep(field("member", $.union_member)),
        repeat(field("method", $.function_definition)),
      // )),
      "}",
    ),
    union_member: $ => seq(
      field("name", $.ident),
      ":",
      field("type", $.type),
    ),

    while_statement: $ => seq(
      "while",
      "(",
      field("condition", $.expression),
      ")",
      optional(field("capture", $.capture)),
      field("body", $._statement),
    ),

    for_statement: $ => seq(
      "for",
      "(",
      field("iterator", $.expression),
      ")",
      optional(field("capture", $.capture)),
      field("body", $._statement),
    ),

    if_statement: $ => prec.right(1, seq(
      "if",
      "(",
      field("condition", $.expression),
      ")",
      optional(field("capture", $.capture)),
      field("body", $._statement),
      optional(field("else", seq(
        "else",
        $._statement,
      ))),
    )),

    return_statement: $ => seq(
      "return",
      optional(field("return_value", $.expression)),
      ";",
    ),

    capture: $ => seq(
      "|",
      field("identifier", $.ident),
      "|",
    ),

    variable_signature: $ => seq(
      field("name", $.ident),
      ":",
      field("type", $.type),
    ),

    type: $ => choice(
      $.ident_type,
      $.primitive_type,
      $.optional_type,
      $.reference_type,
      $.array_type,
      $.error_union,
      $.function_type
    ),
    ident_type: $ => alias($.ident, $.ident_type),

    primitive_type: $ => choice(
      "i8",
      "i16",
      "i32",
      "i64",
      "u8",
      "u16",
      "u32",
      "u64",
      "f32",
      "f64",
      "void",
      "bool",
      "type",
    ),

    optional_type: $ => prec.right(8, seq(
      "?",
      field("inner", $.type),
    )),

    reference_type: $ => prec.right(8, seq(
      "&",
      optional("mut"),
      field("inner", $.type),
    )),

    array_type: $ => seq(
      "[",
      optional(field("size", $.expression)),
      "]",
      field("inner", $.type),
    ),

    error_union: $ => prec.left(4, seq(
      optional(field("error", $.type)),
      "!",
      field("success", $.type),
    )),

    function_type: $ => seq(
      "fn",
      field("params", $.parameter_list),
      field("return_type", $.type),
    ),


    block: $ => seq(
      "{",
      repeat($._statement),
      "}",
    ),

    call_expression: $ => prec(10, seq(
      field("callee", $.expression),
      field("arguments", $.argument_list),
    )),

    argument_list: $ => seq(
      "(",
      repeat(seq(
        field("argument", $.expression),
        ",",
      )),
      optional(field("argument", $.expression)),
      ")",
    ),

    member_expression: $ => prec.left(11, seq(
      field("lhs", $.expression),
      ".",
      field("rhs", choice(
        $.ident,
        $.call_expression,
        $.expression,
      )),
    )),

    binary_expression: $ => choice(
      prec.left(6, seq(field("lhs", $.expression), field("op", "+"), field("rhs", $.expression))),
      prec.left(6, seq(field("lhs", $.expression), field("op", "-"), field("rhs", $.expression))),
      prec.left(7, seq(field("lhs", $.expression), field("op", "*"), field("rhs", $.expression))),
      prec.left(7, seq(field("lhs", $.expression), field("op", "/"), field("rhs", $.expression))),
      prec.left(7, seq(field("lhs", $.expression), field("op", "%"), field("rhs", $.expression))),
      prec.left(5, seq(field("lhs", $.expression), field("op", "=="), field("rhs", $.expression))),
      prec.left(5, seq(field("lhs", $.expression), field("op", ">"), field("rhs", $.expression))),
      prec.left(5, seq(field("lhs", $.expression), field("op", "<"), field("rhs", $.expression))),
      prec.left(5, seq(field("lhs", $.expression), field("op", ">="), field("rhs", $.expression))),
      prec.left(5, seq(field("lhs", $.expression), field("op", "<="), field("rhs", $.expression))),
      prec.left(5, seq(field("lhs", $.expression), field("op", "!="), field("rhs", $.expression))),
      prec.left(7, seq(field("lhs", $.expression), field("op", "&"), field("rhs", $.expression))),
      prec.left(6, seq(field("lhs", $.expression), field("op", "|"), field("rhs", $.expression))),
      prec.left(6, seq(field("lhs", $.expression), field("op", "^"), field("rhs", $.expression))),
      prec.left(4, seq(field("lhs", $.expression), field("op", "and"), field("rhs", $.expression))),
      prec.left(4, seq(field("lhs", $.expression), field("op", "or"), field("rhs", $.expression))),
      prec.left(7, seq(field("lhs", $.expression), field("op", ">>"), field("rhs", $.expression))),
      prec.left(7, seq(field("lhs", $.expression), field("op", "<<"), field("rhs", $.expression))),
    ),

    prefix_expression: $ => prec.right(8, seq(
      field("op", $.prefix_operator),
      field("rhs", $.expression),
    )),

    assignment_expression: $ => prec.right(3, seq(
      field("assignee", $.expression),
      field("op", $.assignment_operator),
      field("rhs", $.expression),
    )),

    if_expression: $ => prec.right(2, seq(
      "if",
      "(",
      field("condition", $.expression),
      ")",
      optional(field("capture", $.capture)),
      field("body", $.expression),
      optional(field("else", seq(
        "else",
        $.expression,
      ))),
    )),

    struct_instantiation_expression: $ => prec(10, seq(
      field("name", $.ident_type),
      "{",
      commaSep(field("member", $.struct_instantiation_expression_member)),
      "}",
    )),
    struct_instantiation_expression_member: $ => seq(
      field("member_name", $.ident),
      ":",
      field("member_value", $.expression),
    ),

    array_instantiation_expression: $ => seq(
      "[",
      optional(field("size", $.expression)),
      "]",
      $.type,
      "{",
      field("initializer_list", commaSep($.expression)),
      "}",
    ),

    range_expression: $ => prec.left(5, seq(
      field("start", $.expression),
      choice("..", "..="),
      field("end", $.expression),
    )),

    reference_expression: $ => prec.right(9, seq(
      "&",
      optional("mut"),
      field("inner", $.expression),
    )),

    parameter_list: $ => seq(
      "(",
      commaSep($.variable_signature),
      ")",
    ),

    prefix_operator: $ => choice(
      "-",
      "!",
    ),

    assignment_operator: $ => choice(
      "=",
      "+=",
      "-=",
      "*=",
      "/=",
      "%=",
      "&=",
      "|=",
      "^=",
      ">>=",
      "<<=",
    ),

    ident: $ => /[a-zA-Z_]+[a-zA-Z0-9_]*/,

    string_literal: $ => seq(
      '"',
      repeat(choice($.escape_sequence, /[^"\\]+/)),
      token.immediate('"')
    ),

    escape_sequence: $ => token.immediate(
      seq(
        "\\",
        choice(
          /[^xu\n]/,
          /u[0-9a-fA-F]{4}/,
          /u\{[0-9a-fA-F]+\}/,
          /x[0-9a-fA-F]{2}/
        )
      )
    ),

    char_literal: $ => seq("'", /./, "'"),
    number_literal: $ => /[+-]?([0-9]*[.])?[0-9]+/,
    comment: $ => token(seq('//', /.*/)),

    true_literal: $ => "true",
    false_literal: $ => "false",
    null_literal: $ => "null",
    undefined_literal: $ => "undefined",
  },
});

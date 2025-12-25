/**
  * @file Zag grammar for tree-sitter
  * @author Dani Tasada <daniteeem@gmail.com>
  * @license MIT
  */

  /// <reference types="tree-sitter-cli/dsl" />
  // @ts-check

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

    expression_statement: $ => seq($._expression, ";"),

    _expression: $ => choice(
      $.ident,
      $.string,
      $.char,
      $.int,
      $.float,

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

    expression: $ => $._expression,

    function_definition: $ => seq(
      "fn",
      field("name", $.ident),
      field("parameters", $.parameter_list),
      field("return_type", $._type),
      field("body", $.block),
    ),

    variable_definition: $ => seq(
      "let",
      optional("mut"),
      field("variable_name", $.ident),
      optional(seq(
        ":",
        field("type", $._type),
      )),
      "=",
      field("assigned_value", $._expression),
      ";",
    ),

    struct_declaration: $ => seq(
      "struct",
      field("name", $.ident),
      optional(field("generics", $.parameter_list)),
      "{",
      repeat(seq(
        field("member_name", $.ident),
        ":",
        field("member_type", $._type),
        optional(seq(
          "=",
          field("default_value", $._expression),
        )),
        ",",
      )),
      "}",
    ),

    enum_declaration: $ => seq(
      "enum",
      optional(field("type", $.parameter_list)),
      field("name", $.ident),
      "{",
      repeat(seq(
        field("member_name", $.ident),
        optional(seq(
          "=",
          field("value", $._expression),
        )),
        ",",
      )),
      "}",
    ),

    union_declaration: $ => seq(
      "union",
      field("name", $.ident),
      "{",
      repeat(seq(
        field("member_name", $.ident),
        ":",
        field("member_type", $._type),
        ",",
      )),
      "}",
    ),

    while_statement: $ => seq(
      "while",
      "(",
      field("condition", $._expression),
      ")",
      optional(field("capture", $.capture)),
      field("body", $._statement),
    ),

    for_statement: $ => seq(
      "for",
      "(",
      field("iterator", $._expression),
      ")",
      optional(field("capture", $.capture)),
      field("body", $._statement),
    ),

    if_statement: $ => prec.right(1, seq(
      "if",
      "(",
      field("condition", $._expression),
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
      optional(field("return_value", $._expression)),
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
      field("type", $._type),
    ),

    type: $ => $._type,

    _type: $ => choice(
      $.ident,
      $.optional_type,
      $.reference_type,
      $.array_type,
      $.error_union,
      $.function_type
    ),

    optional_type: $ => prec.right(8, seq(
      "?",
      field("inner", $._type),
    )),

    reference_type: $ => prec.right(8, seq(
      "&",
      optional("mut"),
      field("inner", $._type),
    )),

    array_type: $ => seq(
      "[",
      optional(field("size", $._expression)),
      "]",
      field("inner", $._type),
    ),

    error_union: $ => prec.left(4, seq(
      optional(field("error", $._type)),
      "!",
      field("success", $._type),
    )),

    function_type: $ => seq(
      "fn",
      field("params", $.parameter_list),
      field("return_type", $._type),
    ),


    block: $ => seq(
      "{",
      repeat($._statement),
      "}",
    ),

    call_expression: $ => prec(10, seq(
      field("lhs", $._expression),
      "(",
      repeat(seq(
        field("argument", $._expression),
        ",",
      )),
      ")",
    )),

    member_expression: $ => prec.left(11, seq(
      field("lhs", $._expression),
      ".",
      field("rhs", $._expression),
    )),

    binary_expression: $ => choice(
      prec.left(6, seq(field("lhs", $._expression), field("op", "+"), field("rhs", $._expression))),
      prec.left(6, seq(field("lhs", $._expression), field("op", "-"), field("rhs", $._expression))),
      prec.left(7, seq(field("lhs", $._expression), field("op", "*"), field("rhs", $._expression))),
      prec.left(7, seq(field("lhs", $._expression), field("op", "/"), field("rhs", $._expression))),
      prec.left(7, seq(field("lhs", $._expression), field("op", "%"), field("rhs", $._expression))),
      prec.left(5, seq(field("lhs", $._expression), field("op", "=="), field("rhs", $._expression))),
      prec.left(5, seq(field("lhs", $._expression), field("op", ">"), field("rhs", $._expression))),
      prec.left(5, seq(field("lhs", $._expression), field("op", "<"), field("rhs", $._expression))),
      prec.left(5, seq(field("lhs", $._expression), field("op", ">="), field("rhs", $._expression))),
      prec.left(5, seq(field("lhs", $._expression), field("op", "<="), field("rhs", $._expression))),
      prec.left(5, seq(field("lhs", $._expression), field("op", "!="), field("rhs", $._expression))),
      prec.left(7, seq(field("lhs", $._expression), field("op", "&"), field("rhs", $._expression))),
      prec.left(6, seq(field("lhs", $._expression), field("op", "|"), field("rhs", $._expression))),
      prec.left(6, seq(field("lhs", $._expression), field("op", "^"), field("rhs", $._expression))),
      prec.left(4, seq(field("lhs", $._expression), field("op", "and"), field("rhs", $._expression))),
      prec.left(4, seq(field("lhs", $._expression), field("op", "or"), field("rhs", $._expression))),
      prec.left(7, seq(field("lhs", $._expression), field("op", ">>"), field("rhs", $._expression))),
      prec.left(7, seq(field("lhs", $._expression), field("op", "<<"), field("rhs", $._expression))),
    ),

    prefix_expression: $ => prec.right(8, seq(
      field("op", $.prefix_operator),
      field("rhs", $._expression),
    )),

    assignment_expression: $ => prec.right(3, seq(
      field("assignee", $._expression),
      field("op", $.assignment_operator),
      field("rhs", $._expression),
    )),

    if_expression: $ => prec.right(2, seq(
      "if",
      "(",
      field("condition", $._expression),
      ")",
      optional(field("capture", $.capture)),
      field("body", $._expression),
      optional(field("else", seq(
        "else",
        $._expression,
      ))),
    )),

    struct_instantiation_expression: $ => prec(10, seq(
      field("name", $.ident),
      "{",
      repeat(seq(
        field("member_name", $.ident),
        ":",
        field("member_value", $._expression),
        ",",
      )),
      "}",
    )),

    array_instantiation_expression: $ => seq(
      "[",
      optional(field("size", $._expression)),
      "]",
      "{",
      repeat(seq(
        field("initializer_list", $._expression),
        ",",
      )),
      "}",
    ),

    range_expression: $ => prec.left(5, seq(
      field("start", $._expression),
      choice("..", "..="),
      field("end", $._expression),
    )),

    reference_expression: $ => prec.right(9, seq(
      "&",
      optional("mut"),
      field("inner", $._expression),
    )),

    parameter_list: $ => seq(
      "(",
      repeat(seq(
        $.variable_signature,
        ",",
      )),
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

    string: $ => seq('"', /.*/, '"'),
    char: $ => seq("'", /./, "'"),
    int: $ => /[+-]?\d+/,
    float: $ => /[+-]?([0-9]*[.])?[0-9]+/,
    comment: $ => token(seq('//', /.*/)),
  },
});

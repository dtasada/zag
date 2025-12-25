/**
  * @file Zag grammar for tree-sitter
  * @author Dani Tasada <daniteeem@gmail.com>
  * @license MIT
  */

  /// <reference types="tree-sitter-cli/dsl" />
  // @ts-check

export default grammar({
  name: "zag",

  rules: {
    // TODO: add the actual grammar rules
    root_node: $ => repeat($.statement),

    statement: $ => choice(
      $.return_statement,
      $.expression,
      $.variable_definition,
      $.struct_declaration,
      $.enum_declaration,
      $.union_declaration,
      $.function_definition,
      $.block,
      $.if_statement,
      $.while_statement,
      $.for_statement,
    ),

    expression: $ => choice(
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
      $.if_statement,
      $.range_expression,
      $.reference_expression,
    ),


    function_definition: $ => seq(
      field("name", $.ident),
      field("parameters", $.parameter_list),
      field("return_type", $.type),
      field("body", $.block),
    ),

    statement: $ => choice(
      field("return_statement", $.return_statement),
      field("expression", $.expression),
      $.variable_definition,
      $.struct_declaration,
      $.enum_declaration,
      $.union_declaration,
      $.function_definition,
      $.block,
      $.if_statement,
      $.while_statement,
      $.for_statement,

      $.struct_declaration,
      $.enum_declaration,
      $.union_declaration,
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
      field("name", $.ident),
      optional(field("generics", $.parameter_list)),
      "{",
          repeat(seq(field("member",
            field("name", $.ident),
            ":",
            field("type", $.type),
            optional(seq(
              "=",
              field("default_value", $.expression),
            ))
          ))),
          "}",
    ),

    enum_declaration: $ => seq(
      "enum",
      optional(field("type", $.parameter_list)),
      field("name", $.ident),
      "{",
          repeat(seq(field("member",
            field("name", $.ident),
            optional(seq(
              "=",
              field("value", $.expression),
            ))
          ))),
          "}",
    ),

    union_declaration: $ => seq(
      "union",
      field("name", $.ident),
      "{",
          repeat(seq(field("member",
            field("name", $.ident),
            ":",
            field("type", $.type),
          ))),
          "}",
    ),

    while_statement: $ => seq(
      "while",
      "(",
        field("condition", $.expression),
        ")",
      optional(field("capture", $.capture)),
      field("body", $.statement),
    ),

    for_statement: $ => seq(
      "for",
      "(",
        field("iterator", $.expression),
        ")",
      optional(field("capture", $.capture)),
      field("body", $.statement),
    ),

    if_statement: $ => seq(
      "if",
      "(",
        field("condition", $.expression),
        ")",
      optional(field("capture", $.capture)),
      field("body", $.statement),
      optional(field("else", seq(
        "else",
        $.statement,
      ))),
    ),

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
      field("type", $.type),
    ),

    type: $ => choice(
      field("symbol", $.ident),
      field("optional", seq(
        "?",
        field("inner", $.type),
      )),
      field("reference", seq(
        "&",
        optional("mut"),
        field("inner", $.type),
      )),
      field("array", seq(
        "[",
          optional(field("size", $.expression)),
          "]",
        field("inner", $.type),
      )),
      field("error_union", seq(
        optional(field("error", $.type)),
        "!",
        field("success", $.type),
      )),
      field("function", seq(
        "fn",
        field("params", $.parameter_list),
        field("return_type", $.type),
      ))
    ),

    block: $ => seq(
      "{",
          repeat($.statement),
          "}",
    ),

    call_expression: $ => seq(
      field("lhs", $.expression),
      "(",
        repeat(field("argument", $.expression)),
        ")",
    ),

    member_expression: $ => seq(
      field("lhs", $.expression),
      ".",
      field("rhs", $.expression),
    ),

    binary_expression: $ => seq(
      field("lhs", $.expression),
      field("op", $.binary_operator),
      field("rhs", $.expression),
    ),

    prefix_expression: $ => seq(
      field("op", $.prefix_operator),
      field("rhs", $.expression),
    ),

    assignment_expression: $ => seq(
      field("assignee", $.expression),
      field("op", $.assignment_operator),
      field("rhs", $.expression),
    ),

    struct_instantiation_expression: $ => seq(
      field("name", $.ident),
      "{",
          repeat(field("member",
            field("member_name", $.ident),
            ":",
            field("member_value", $.expression),
          )),
          "}",
    ),

    array_instantiation_expression: $ => seq(
      "[",
        optional(field("size", $.expression)),
        "]",
      "{",
          repeat(seq(
            field("initializer_list", $.expression),
            ",",
          )),
          "}",
    ),

    range_expression: $ => seq(
      field("start", $.expression),
      choice("..", "..="),
      field("end", $.expression),
    ),

    reference_expression: $ => seq(
      "&",
      optional("mut"),
      field("inner", $.expression),
    ),

    parameter_list: $ => seq(
      "(",
        repeat(field("parameter", $.variable_signature)),
        ")",
    ),

    binary_operator: $ => choice(
      "+",
      "-",
      "*",
      "/",
      "%",

      "==",
      ">",
      "<",
      ">=",
      "<=",
      "!=",

      "&",
      "|",
      "^",
      "and",
      "or",
      ">>",
      "<<",
    ),

    prefix_operator: $ => choice(
      "-",
      "!",
    ),

    assignment_operator: $ => choice(
      "==",
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
  },
});

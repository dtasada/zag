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

function commaSep1(item) {
  return choice(
    item,
    seq(repeat1(seq(item, ",")), optional(item)),
  );
}

export default grammar({
  name: "zag",

  extras: $ => [
    /\s+/,
    $.comment,
  ],

  conflicts: $ => [
    [$._statement, $.expression],
    [$.expression, $.ident_type],
    [$.expression, $.call_callee],
    [$.type, $.generic_type],
    [$.expression, $.ident_type, $.call_callee],
    [$.expression, $.generic_argument_list],
    [$.expression],
    [$.struct_declaration],
    [$.enum_declaration],
    [$.union_declaration],
    [$.try_expression, $.index_expression],
    [$.try_expression, $.generic_expression],
    [$.try_expression, $.catch_expression],
    [$.catch_expression],
    [$.catch_expression, $.index_expression],
    [$.catch_expression, $.generic_expression],
    [$.type, $.member_type],
    [$._statement, $.index_expression],
    [$.slice_type, $.array_instantiation_expression],
    [$.array_type, $.array_instantiation_expression],
  ],

  rules: {
    source_file: $ => repeat($._statement),

    _statement: $ => choice(
      $.return_statement,
      $.import_statement,
      $.variable_definition,
      $.struct_declaration,
      $.enum_declaration,
      $.union_declaration,
      $.function_definition,
      $.binding_function_declaration,
      $.binding_type_declaration,
      $.if_statement,
      $.while_statement,
      $.for_statement,
      $.expression_statement,
      $.match_expression,
      $.break_statement,
      $.continue_statement,
      $.block,
      $.defer_statement,
      $.expression,
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
      $.c_null_literal,
      $.undefined_literal,

      $.try_expression,
      $.catch_expression,
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
      $.match_expression,
      $.index_expression,
      $.generic_expression,
      $.type,
    ),

    defer_statement: $ => seq("defer", $._statement),

    break_statement: $ => seq("break", ";"),
    continue_statement: $ => seq("continue", ";"),

    try_expression: $ => seq("try", $.expression),
    catch_expression: $ => seq($.expression, "catch", $.expression),

    generic_expression: $ => seq(
      $.expression,
      $.generic_argument_list,
    ),

    index_expression: $ => seq(
      $.expression,
      "[",
      choice(
        $.expression,
        seq(choice("..", "..="), field("end", $.expression)),
      ),
      "]",
    ),

    match_expression: $ => seq(
      "match",
      "(",
      $.expression,
      ")",
      "{",
      commaSep(seq(
        choice(
          commaSep($.expression),
          "else",
        ),
        "->",
        $._statement,
      )),
      "}",
    ),

    function_definition: $ => seq(
      optional("pub"),
      "fn",
      field("name", $.ident),
      optional(field("generic_parameters", $.generic_parameter_list)),
      field("parameters", $.parameter_list),
      field("return_type", $.type),
      choice(
        field("body", $.block),
        seq("->", $._statement),
      ),
    ),

    binding_function_declaration: $ => seq(
      optional("pub"),
      "bind",
      "fn",
      field("name", $.ident),
      field("parameters", $.parameter_list),
      field("return_type", $.type),
      ";",
    ),

    binding_type_declaration: $ => seq(
      optional("pub"),
      "bind",
      choice("struct", "enum", "union"),
      field("name", $.ident),
      ";",
    ),

    variable_definition: $ => seq(
      choice(
        seq(
          "let",
          optional("mut"),
        ),
        "const",
      ),
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
      optional("pub"),
      "struct",
      field("name", $.ident_type),
      optional(field("generic_parameters", $.generic_parameter_list)),
      "{",
      repeat(choice(
        field("variable", $.variable_definition),
        commaSep1(field("member", $.struct_member)),
        field("method", $.function_definition),
        field("subtype", $.struct_declaration),
        field("subtype", $.union_declaration),
        field("subtype", $.enum_declaration),
      )),
      "}",
    ),

    struct_member: $ => seq(
      commaSep1(field("name", $.ident)),
      ":",
      field("type", $.type),
    ),

    enum_declaration: $ => seq(
      optional("pub"),
      "enum",
      optional(field("type", $.parameter_list)),
      field("name", $.ident_type),
      "{",
      repeat(choice(
        field("variable", $.variable_definition),
        commaSep1(field("member", $.enum_member)),
        field("method", $.function_definition),
        field("subtype", $.struct_declaration),
        field("subtype", $.union_declaration),
        field("subtype", $.enum_declaration),
      )),
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
      optional("pub"),
      "union",
      field("name", $.ident_type),
      optional(field("generic_parameters", $.generic_parameter_list)),
      "{",
      repeat(choice(
        field("variable", $.variable_definition),
        commaSep1(field("member", $.union_member)),
        field("method", $.function_definition),
        field("subtype", $.struct_declaration),
        field("subtype", $.union_declaration),
        field("subtype", $.enum_declaration),
      )),
      "}",
    ),
    union_member: $ => seq(
      commaSep1(field("name", $.ident)),
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

    import_statement: $ => seq(
      "import",
      seq(
        optional(repeat(seq(
          $.ident,
          ".",
        ))),
        $.ident,
      ),
      optional(seq(
        "as",
        $.ident,
      )),
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
      $.generic_type,
      $.primitive_type,
      $.optional_type,
      $.reference_type,
      $.array_type,
      $.slice_type,
      $.error_union,
      $.function_type,
      $.member_type,
    ),

    member_type: $ => seq($.ident_type, ".", $.ident_type),

    ident_type: $ => alias($.ident, $.ident_type),

    generic_type: $ => seq(
      $.ident_type,
      $.generic_parameter_list,
    ),

    primitive_type: $ => choice(
      "i8",
      "i16",
      "i32",
      "i64",
      "u8",
      "u16",
      "u32",
      "u64",
      "usize",
      "f32",
      "f64",
      "void",
      "bool",
      "type",

      "c_char",
      "c_int",
      "c_short",
      "c_long",

      "c_uchar",
      "c_uint",
      "c_ushort",
      "c_ulong",
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
      field("size", $.expression),
      "]",
      field("inner", $.type),
    ),

    slice_type: $ => seq(
      "[",
      "]",
      optional("mut"),
      field("inner", $.type),
    ),

    error_union: $ => prec.left(4, seq(
      optional(field("failure", $.type)),
      "!",
      field("success", $.type),
    )),

    function_type: $ => seq(
      "fn",
      optional($.generic_parameter_list),
      field("params", $.parameter_list),
      field("return_type", $.type),
    ),


    block: $ => seq(
      "{",
      repeat($._statement),
      "}",
    ),

    call_expression: $ => seq(
      field("callee", $.call_callee),
      field("arguments", $.argument_list),
    ),

    call_callee: $ => choice(
      $.ident,
      $.member_expression,
      $.call_expression,
      $.prefix_expression,
      $.reference_expression,
      $.struct_instantiation_expression,
      $.array_instantiation_expression,
      $.if_expression,
      $.range_expression,
      $.match_expression,
      $.block,
    ),

    argument_list: $ => seq(
      "(",
      commaSep(field("argument", $.expression)),
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
      prec.left(4, seq(field("lhs", $.expression), field("op", "but"), field("rhs", $.expression))),
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
      optional($.generic_argument_list),
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
      field("end", optional($.expression)),
    )),

    reference_expression: $ => prec.right(9, seq(
      "&",
      optional("mut"),
      field("inner", $.expression),
    )),

    parameter_list: $ => seq(
      "(",
      optional(seq(
        optional("mut"),
        commaSep(choice(
          $.variable_signature,
          field("name", $.ident),
        )),
        choice(
          $.variable_signature,
          seq(
            field("name", $.ident),
            "...",
          )
        ),
      )),
      ")",
    ),

    generic_parameter_list: $ => seq(
      "<",
      commaSep(choice(field("name", $.ident_type), $.variable_signature)),
      ">",
    ),

    generic_argument_list: $ => prec.dynamic(2, seq(
      token.immediate("<"),
      commaSep(field("argument", choice($.type, $.expression))),
      token.immediate(">"),
    )),

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
    number_literal: $ => choice($.integer, $.float),
    comment: $ => token(seq('//', /.*/)),

    integer: _ => {
      const separator = '_';
      const hex = /[0-9A-Fa-f]/;
      const oct = /[0-7]/;
      const bin = /[0-1]/;
      const decimal = /[0-9]/;
      const hexDigits = seq(repeat1(hex), repeat(seq(separator, repeat1(hex))));
      const octDigits = seq(repeat1(oct), repeat(seq(separator, repeat1(oct))));
      const binDigits = seq(repeat1(bin), repeat(seq(separator, repeat1(bin))));
      const decimalDigits = seq(repeat1(decimal), repeat(seq(separator, repeat1(decimal))));

      return token(choice(
        seq('0x', hexDigits),
        seq('0o', octDigits),
        seq('0b', binDigits),
        decimalDigits,
      ));
    },

    float: _ => {
      const separator = '_';
      const hex = /[0-9A-Fa-f]/;
      const decimal = /[0-9]/;
      const hexDigits = seq(repeat1(hex), repeat(seq(separator, repeat1(hex))));
      const decimalDigits = seq(repeat1(decimal), repeat(seq(separator, repeat1(decimal))));

      return token(choice(
        seq('0x', hexDigits, '.', hexDigits, optional(seq(/[pP][+-]?/, decimalDigits))),
        seq(decimalDigits, '.', decimalDigits, optional(seq(/[eE][+-]?/, decimalDigits))),
        seq('0x', hexDigits, /[pP][+-]?/, decimalDigits),
        seq(decimalDigits, /[eE][+-]?/, decimalDigits),
      ));
    },

    true_literal: $ => "true",
    false_literal: $ => "false",
    null_literal: $ => "null",
    c_null_literal: $ => "c_null",
    undefined_literal: $ => "undefined",
  },
});

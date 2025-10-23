const std = @import("std");

const LexerToken = @import("Lexer.zig").Token;

pub const ParameterList = std.ArrayList(VariableSignature);
pub const ArgumentList = std.ArrayList(Expression);
// pub const RootNode = std.ArrayList(TopLevelNode);
pub const RootNode = std.ArrayList(Statement); // statement instead of toplevelnode for debugging
pub const Block = std.ArrayList(Statement);

pub const BinaryOperator = enum {
    plus,
    dash,
    asterisk,
    slash,
    percent,

    plus_equals,
    minus_equals,
    times_equals,
    slash_equals,
    mod_equals,
    and_equals,
    or_equals,
    xor_equals,

    equals,
    equals_equals,
    greater,
    less,
    greater_equals,
    less_equals,
    bang_equals,

    ampersand,
    pipe,
    caret,
    logical_and,
    logical_or,

    pub fn fromLexerToken(t: LexerToken) BinaryOperator {
        return std.meta.stringToEnum(BinaryOperator, @tagName(std.meta.activeTag(t))) orelse
            @panic("called BinaryOperator.fromLexerToken on Lexer.Token that is not a binary operator");
        // return switch (t) {
        //     .plus => .plus,
        //     .dash => .dash,
        //     .asterisk => .asterisk,
        //     .slash => .slash,
        //     .percent => .percent,
        //
        //     .plus_equals => .plus_equals,
        //     .minus_equals => .minus_equals,
        //     .times_equals => .times_equals,
        //     .slash_equals => .slash_equals,
        //     .mod_equals => .mod_equals,
        //     .and_equals => .and_equals,
        //     .or_equals => .or_equals,
        //     .xor_equals => .xor_equals,
        //
        //     .equals => .equals,
        //     .equals_equals => .equals_equals,
        //     .greater => .greater,
        //     .less => .less,
        //     .greater_equals => .greater_equals,
        //     .less_equals => .less_equals,
        //     .bang_equals => .bang_equals,
        //
        //     .ampersand => .ampersand,
        //     .pipe => .pipe,
        //     .caret => .caret,
        //     .logical_and => .logical_and,
        //     .logical_or => .logical_or,
        // };
    }
};

pub const Expression = union(enum) {
    pub const Binary = struct {
        lhs: *const Expression,
        op: BinaryOperator,
        rhs: *const Expression,
    };

    pub const Member = struct {
        lhs: *const Expression,
        rhs: *const Expression,
    };

    pub const Call = struct {
        callee: *const Expression,
        args: ArgumentList,
    };

    pub const Prefix = struct {
        op: LexerToken,
        rhs: *const Expression,
    };

    pub const Assignment = struct {
        assignee: *const Expression,
        op: LexerToken,
        value: *const Expression,
    };

    bad_node,

    // literals
    ident: []const u8,
    string: []const u8,
    int: i64,
    uint: u64,
    float: f64,

    call: Call,
    member: Member,
    binary: Binary,
    prefix: Prefix,
    assignment: Assignment,
};

pub const TopLevelNode = union(enum) {
    bin_expr: Expression.Binary,
    func_def: FunctionDefinition,
};

pub const FunctionDefinition = struct {
    function_name: []const u8,
    params: ParameterList = .{},
    return_type: Type,
    body: Block,
};

pub const Statement = union(enum) {
    @"return": Expression,
    expression: Expression,
    variable_declaration: VariableDeclarationStatement,
};

const VariableDeclarationStatement = struct {
    variable_name: []const u8,
    is_mut: bool,
    assigned_value: Expression,
    type: Type,
};

const VariableSignature = struct {
    param_name: []const u8,
    type: Type,
};

pub const Type = union(enum) {
    inferred,
    symbol: []const u8,
    array: *const Type,
};

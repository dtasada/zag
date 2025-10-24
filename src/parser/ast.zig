const std = @import("std");

const LexerToken = @import("../Lexer.zig").Token;

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

    pub const StructInstantiation = struct {
        name: []const u8,
        members: std.StringHashMap(Expression),
    };

    pub const ArrayInstantiation = struct {
        type: Type,
        contents: std.ArrayList(Expression) = .{},
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
    struct_instantiation: StructInstantiation,
    array_instantiation: ArrayInstantiation,
    block: Block,
    @"if": IfExpression,
};

pub const TopLevelNode = union(enum) {
    bin_expr: Expression.Binary,
    func_def: FunctionDefinition,
};

pub const FunctionDefinition = struct {
    name: []const u8,
    parameters: ParameterList = .{},
    return_type: Type,
    body: Block,
};

pub const Statement = union(enum) {
    const VariableDeclaration = struct {
        is_mut: bool,
        variable_name: []const u8,
        type: Type,
        assigned_value: Expression,
    };

    const StructDeclaration = struct {
        const Field = struct {
            name: []const u8,
            type: Type,
        };

        name: []const u8,
        members: std.ArrayList(Field) = .{},
        methods: std.ArrayList(FunctionDefinition) = .{},
    };

    const While = struct {
        condition: *const Expression,
        capture: ?[]const u8,
        body: *const Expression,
    };

    @"return": Expression,
    expression: Expression,
    variable_declaration: VariableDeclaration,
    struct_declaration: StructDeclaration,
    function_definition: FunctionDefinition,
    @"if": IfExpression,
    @"while": While,
};

pub const IfExpression = struct {
    condition: *const Expression,
    capture: ?[]const u8,
    body: *const Expression,
    @"else": ?*const Expression,
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

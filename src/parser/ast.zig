const std = @import("std");
const utils = @import("utils");

const LexerToken = @import("Lexer").Token;

pub const ParameterList = std.ArrayList(VariableSignature);
pub const ArgumentList = std.ArrayList(Expression);
pub const RootNode = std.ArrayList(Statement);
pub const Block = std.ArrayList(Statement);

const ast = @This();

pub const BinaryOperator = enum {
    plus,
    dash,
    asterisk,
    slash,
    percent,

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
    shift_right,
    shift_left,

    pub fn fromLexerToken(t: LexerToken) BinaryOperator {
        return std.meta.stringToEnum(BinaryOperator, @tagName(std.meta.activeTag(t))) orelse
            @panic("called BinaryOperator.fromLexerToken on Lexer.Token that is not a binary operator");
    }
};

pub const AssignmentOperator = enum {
    equals,
    plus_equals,
    minus_equals,
    times_equals,
    slash_equals,
    mod_equals,
    and_equals,
    or_equals,
    xor_equals,
    shift_right_equals,
    shift_left_equals,

    pub fn fromLexerToken(t: LexerToken) AssignmentOperator {
        return std.meta.stringToEnum(AssignmentOperator, @tagName(std.meta.activeTag(t))) orelse
            @panic("called AssignmentOperator.fromLexerToken on Lexer.Token that is not an assignment operator");
    }
};

pub const PrefixOperator = enum {
    dash,
    bang,

    pub fn fromLexerToken(t: LexerToken) PrefixOperator {
        return std.meta.stringToEnum(PrefixOperator, @tagName(std.meta.activeTag(t))) orelse
            @panic("called PrefixOperator.fromLexerToken on Lexer.Token that is not a prefix operator");
    }
};

pub const Expression = union(enum) {
    bad_node: struct { pos: utils.Position },

    // literals
    ident: struct { pos: utils.Position, ident: []const u8 },
    string: struct { pos: utils.Position, string: []const u8 },
    char: struct { pos: utils.Position, char: u8 },
    int: struct { pos: utils.Position, int: i64 },
    uint: struct { pos: utils.Position, uint: u64 },
    float: struct { pos: utils.Position, float: f64 },

    call: Call,
    member: Member,
    binary: Binary,
    prefix: Prefix,
    assignment: Assignment,
    struct_instantiation: StructInstantiation,
    array_instantiation: ArrayInstantiation,
    block: Expression.Block,
    @"if": If,
    range: Range,
    index: Index,
    reference: Reference,

    pub const Block = struct { pos: utils.Position, block: ast.Block };

    pub const Binary = struct {
        pos: utils.Position,
        lhs: *const Expression,
        op: BinaryOperator,
        rhs: *const Expression,
    };

    pub const Member = struct {
        pos: utils.Position,
        parent: *const Expression,
        member_name: []const u8,
    };

    pub const Call = struct {
        pos: utils.Position,
        callee: *const Expression,
        args: ArgumentList,
    };

    pub const Prefix = struct {
        pos: utils.Position,
        op: PrefixOperator,
        rhs: *const Expression,
    };

    pub const Assignment = struct {
        pos: utils.Position,
        assignee: *const Expression,
        op: AssignmentOperator,
        value: *const Expression,
    };

    pub const StructInstantiation = struct {
        pos: utils.Position,
        name: []const u8,
        members: std.StringHashMap(Expression),
    };

    pub const ArrayInstantiation = struct {
        pos: utils.Position,
        length: *const Expression,
        type: Type,
        contents: std.ArrayList(Expression) = .empty,
    };

    const Range = struct {
        pos: utils.Position,
        start: *const Expression,
        end: *const Expression,
        inclusive: bool,
    };

    const Reference = struct {
        pos: utils.Position,
        inner: *const Expression,
        is_mut: bool,
    };

    const If = struct {
        pos: utils.Position,
        condition: *const Expression,
        capture: ?[]const u8 = null,
        body: *const Expression,
        @"else": ?*const Expression = null,
    };

    const Index = struct {
        pos: utils.Position,
        lhs: *const Expression,
        index: *const Expression,
    };

    pub inline fn getPosition(self: *const Expression) utils.Position {
        return switch (self.*) {
            inline else => |some| some.pos,
        };
    }
};

pub const Statement = union(enum) {
    @"return": Return,
    expression: Expression,
    variable_definition: VariableDefinition,
    struct_declaration: StructDeclaration,
    enum_declaration: EnumDeclaration,
    union_declaration: UnionDeclaration,
    function_definition: FunctionDefinition,
    block: ast.Expression.Block,
    @"if": If,
    @"while": While,
    @"for": For,

    pub const Return = struct { pos: utils.Position, @"return": ?ast.Expression };

    pub const FunctionDefinition = struct {
        pos: utils.Position,
        name: []const u8,
        parameters: ParameterList = .empty,
        return_type: Type,
        body: ast.Block,

        pub fn getType(self: *const FunctionDefinition) Type {
            return .{
                .function = .{
                    .parameters = self.parameters,
                    .return_type = &self.return_type,
                },
            };
        }
    };

    pub const VariableDefinition = struct {
        pos: utils.Position,
        is_mut: bool,
        variable_name: []const u8,
        type: Type,
        assigned_value: ast.Expression,
    };

    pub const StructDeclaration = struct {
        const Member = struct {
            name: []const u8,
            type: Type,
            default_value: ?ast.Expression = null,
        };

        pos: utils.Position,
        name: []const u8,
        generic_types: ?ParameterList = null,
        members: std.ArrayList(Member) = .empty,
        methods: std.ArrayList(FunctionDefinition) = .empty,
    };

    pub const EnumDeclaration = struct {
        const Member = struct {
            name: []const u8,
            value: ?ast.Expression = null,
        };

        pos: utils.Position,
        name: []const u8,
        members: std.ArrayList(Member) = .empty,
        methods: std.ArrayList(FunctionDefinition) = .empty,
    };

    pub const UnionDeclaration = struct {
        const Member = struct {
            name: []const u8,
            type: ?Type,
        };

        pos: utils.Position,
        name: []const u8,
        generic_types: ?ParameterList = null, // only for structs and unions
        members: std.ArrayList(Member) = .empty,
        methods: std.ArrayList(FunctionDefinition) = .empty,
    };

    pub const While = struct {
        pos: utils.Position,
        condition: *const ast.Expression,
        capture: ?[]const u8 = null,
        body: *const Statement,
    };

    pub const For = struct {
        pos: utils.Position,
        iterator: *const ast.Expression,
        capture: []const u8,
        body: *const Statement,
    };

    pub const If = struct {
        pos: utils.Position,
        condition: *const ast.Expression,
        capture: ?[]const u8 = null,
        body: *const Statement,
        @"else": ?*const Statement = null,
    };
};

pub const VariableSignature = struct {
    name: []const u8,
    type: Type,
};

pub const Type = union(enum) {
    const Reference = struct {
        inner: *const Type,
        is_mut: bool,
    };

    const Array = struct {
        inner: *const Type,
        /// if size is `null` type is an arraylist, else it's an array.
        /// if size is `_`, type is an array of inferred size.
        /// if size is a valid expression, type is an array of specified size.
        size: ?*const Expression = null,
    };

    const ErrorUnion = struct {
        success: *const Type,
        @"error": ?*const Type = null,
    };

    const Function = struct {
        parameters: ParameterList = .empty,
        return_type: *const Type,
    };

    inferred,
    symbol: []const u8,
    optional: *const Type,
    reference: Reference,
    array: Array,
    error_union: ErrorUnion,
    function: Function,
};

const std = @import("std");
const utils = @import("../utils.zig");

const LexerToken = @import("../Lexer.zig").Token;

pub const ParameterList = std.ArrayList(VariableSignature);
pub const ArgumentList = std.ArrayList(Expression);
pub const RootNode = std.ArrayList(Statement);
pub const Block = std.ArrayList(Statement);

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
    bad_node,

    // literals
    ident: []const u8,
    string: []const u8,
    char: u8,
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
    @"if": If,
    range: Range,
    reference: Reference,

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
        op: PrefixOperator,
        rhs: *const Expression,
    };

    pub const Assignment = struct {
        assignee: *const Expression,
        op: AssignmentOperator,
        value: *const Expression,
    };

    pub const StructInstantiation = struct {
        name: []const u8,
        members: std.StringHashMap(Expression),
    };

    pub const ArrayInstantiation = struct {
        type: Type,
        contents: std.ArrayList(Expression) = .empty,
    };

    const Range = struct {
        start: *const Expression,
        end: *const Expression,
        inclusive: bool,
    };

    const Reference = struct {
        inner: *const Expression,
        is_mut: bool,
    };

    const If = struct {
        condition: *const Expression,
        capture: ?[]const u8 = null,
        body: *const Expression,
        @"else": ?*const Expression = null,
    };
};

pub const FunctionDefinition = struct {
    name: []const u8,
    parameters: ParameterList = .empty,
    return_type: Type,
    body: Block,

    pub fn getType(self: *const FunctionDefinition) Type {
        return .{
            .function = .{
                .parameters = self.parameters,
                .return_type = &self.return_type,
            },
        };
    }
};

pub const Statement = union(enum) {
    @"return": ?Expression,
    expression: Expression,
    variable_definition: VariableDefinition,
    struct_declaration: StructDeclaration,
    enum_declaration: EnumDeclaration,
    union_declaration: UnionDeclaration,
    function_definition: FunctionDefinition,
    block: Block,
    @"if": If,
    @"while": While,
    @"for": For,

    pub const VariableDefinition = struct {
        is_mut: bool,
        variable_name: []const u8,
        type: Type,
        assigned_value: Expression,
    };

    fn CompoundType(@"type": enum { @"struct", @"enum", @"union" }) type {
        return struct {
            const Field = switch (@"type") {
                .@"struct" => struct {
                    name: []const u8,
                    type: Type,
                    default_value: ?Expression = null,
                },
                .@"enum" => struct {
                    name: []const u8,
                    default_value: ?Expression = null,
                },
                .@"union" => struct {
                    name: []const u8,
                    type: ?Type,
                },
            };

            name: []const u8,
            generic_types: ?ParameterList = null, // only for structs and unions
            members: std.ArrayList(Field) = .empty,
            methods: std.ArrayList(FunctionDefinition) = .empty,
        };
    }

    pub const StructDeclaration = CompoundType(.@"struct");
    pub const EnumDeclaration = CompoundType(.@"enum");
    pub const UnionDeclaration = CompoundType(.@"union");

    pub const While = struct {
        condition: *const Expression,
        capture: ?[]const u8 = null,
        body: *const Statement,
    };

    pub const For = struct {
        iterator: *const Expression,
        capture: []const u8,
        body: *const Statement,
    };

    pub const If = struct {
        condition: *const Expression,
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
    self_type,
    symbol: []const u8,
    optional: *const Type,
    reference: Reference,
    array: Array,
    error_union: ErrorUnion,
    function: Function,
};

//! Declarative description of the AST.

const std = @import("std");
const utils = @import("utils");

const LexerToken = @import("Lexer").Token;

pub const ParameterList = std.ArrayList(VariableSignature);
pub const ArgumentList = std.ArrayList(Expression);
pub const RootNode = std.ArrayList(Statement);
pub const Block = std.ArrayList(Statement);

const ast = @This();

pub const BinaryOperator = enum {
    @"+",
    @"-",
    @"*",
    @"/",
    @"%",

    @"==",
    @">",
    @"<",
    @">=",
    @"<=",
    @"!=",

    @"&",
    @"|",
    @"^",
    @"and",
    but,
    @"or",
    @">>",
    @"<<",

    pub fn fromLexerToken(t: LexerToken) BinaryOperator {
        return std.meta.stringToEnum(BinaryOperator, @tagName(std.meta.activeTag(t))) orelse
            @panic("called BinaryOperator.fromLexerToken on Lexer.Token that is not a binary operator");
    }
};

pub const AssignmentOperator = enum {
    @"=",
    @"+=",
    @"-=",
    @"*=",
    @"/=",
    @"%=",
    @"&=",
    @"|=",
    @"^=",
    @">>=",
    @"<<=",

    pub fn fromLexerToken(t: LexerToken) AssignmentOperator {
        return std.meta.stringToEnum(AssignmentOperator, @tagName(std.meta.activeTag(t))) orelse
            @panic("called AssignmentOperator.fromLexerToken on Lexer.Token that is not an assignment operator");
    }
};

pub const PrefixOperator = enum {
    @"-",
    @"!",

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

    @"if": If,
    array_instantiation: ArrayInstantiation,
    assignment: Assignment,
    binary: Binary,
    block: Expression.Block,
    call: Call,
    comparison: Comparison,
    generic: Generic,
    index: Index,
    slice: Slice,
    match: Match,
    member: Member,
    prefix: Prefix,
    range: Range,
    reference: Reference,
    struct_instantiation: StructInstantiation,
    type: Type,
    @"try": struct { pos: utils.Position, @"try": *const Expression },
    @"catch": struct {
        pos: utils.Position,
        lhs: *const Expression,
        rhs: *const Expression,
    },

    pub const Match = struct {
        pub const Case = struct {
            pub const Condition = union(enum) {
                opts: std.ArrayList(Expression),
                @"else",
            };

            pos: utils.Position,
            condition: Condition,
            result: Statement,
        };

        pos: utils.Position,
        condition: *const Expression,
        cases: std.ArrayList(Case),
    };

    pub const Block = struct { pos: utils.Position, block: ast.Block };
    pub const Generic = struct {
        pos: utils.Position,
        lhs: *const Expression,
        arguments: ArgumentList,
    };

    pub const Binary = struct {
        pos: utils.Position,
        lhs: *const Expression,
        op: BinaryOperator,
        rhs: *const Expression,
    };

    pub const Comparison = struct {
        pub const Item = struct {
            op: BinaryOperator,
            right: *const Expression,
        };

        pos: utils.Position,
        left: *const Expression,
        comparisons: std.ArrayList(Item),
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
        type_expr: *const Expression,
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
        end: ?*const Expression,
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
        capture: ?utils.Capture,
        body: *const Expression,
        @"else": ?*const Expression = null,
    };

    pub const Index = struct {
        pos: utils.Position,
        lhs: *const Expression,
        index: *const Expression,
    };

    pub const Slice = struct {
        pos: utils.Position,
        lhs: *const Expression,
        start: ?*const Expression,
        end: ?*const Expression,
        inclusive: bool,
    };

    pub inline fn getPosition(self: *const Expression) utils.Position {
        return switch (self.*) {
            .type => |t| t.getPosition(),
            inline else => |some| some.pos,
        };
    }
};

pub const Statement = union(enum) {
    @"break",
    @"continue",
    @"for": For,
    @"if": If,
    @"return": Return,
    @"while": While,
    binding_function_declaration: BindingFunctionDeclaration,
    binding_type_declaration: BindingTypeDeclaration,
    block: ast.Expression.Block,
    enum_declaration: EnumDeclaration,
    expression: Expression,
    function_definition: FunctionDefinition,
    import: Import,
    struct_declaration: StructDeclaration,
    union_declaration: UnionDeclaration,
    variable_definition: VariableDefinition,
    block_eval: Expression,
    @"defer": struct { pos: utils.Position, stmt: *const Statement },

    pub const For = struct {
        pos: utils.Position,
        iterator: ast.Expression,
        capture: ?utils.Capture,
        body: *const Statement,
    };

    pub const FunctionDefinition = struct {
        pos: utils.Position,
        is_pub: bool,
        name: []const u8,
        generic_parameters: ParameterList,
        parameters: ParameterList,
        return_type: Type,
        body: ast.Block,

        pub fn getType(self: *const FunctionDefinition) Type {
            return .{
                .function = .{
                    .name = self.name,
                    .pos = self.pos,
                    .generic_parameters = self.generic_parameters,
                    .parameters = self.parameters,
                    .return_type = &self.return_type,
                },
            };
        }
    };

    pub const If = struct {
        pos: utils.Position,
        condition: ast.Expression,
        capture: ?utils.Capture,
        body: *const Statement,
        @"else": ?*const Statement = null,
    };

    pub const Import = struct {
        pos: utils.Position,
        module_name: std.ArrayList([]const u8),
        alias: ?[]const u8,
    };

    pub const Return = struct {
        pos: utils.Position,
        @"return": ?ast.Expression,
    };

    pub const StructDeclaration = struct {
        const Member = struct {
            name: []const u8,
            type: Type,
        };
        pos: utils.Position,
        is_pub: bool,
        name: []const u8,
        generic_types: ParameterList,
        variables: std.ArrayList(VariableDefinition),
        subtypes: std.ArrayList(Subtype),
        members: std.ArrayList(Member),
        methods: std.ArrayList(FunctionDefinition),

        pub fn clone(self: *const StructDeclaration, alloc: std.mem.Allocator) !StructDeclaration {
            return .{
                .pos = self.pos,
                .is_pub = self.is_pub,
                .name = self.name, // notice: doesn't clone the name.
                .generic_types = try self.generic_types.clone(alloc),
                .variables = try self.variables.clone(alloc),
                .subtypes = try self.subtypes.clone(alloc),
                .members = try self.members.clone(alloc),
                .methods = try self.methods.clone(alloc),
            };
        }
    };

    pub const UnionDeclaration = struct {
        const Member = struct {
            name: []const u8,
            type: ?Type,
        };
        pos: utils.Position,
        is_pub: bool,
        name: []const u8,
        generic_types: ParameterList,
        variables: std.ArrayList(VariableDefinition),
        subtypes: std.ArrayList(Subtype),
        members: std.ArrayList(Member),
        methods: std.ArrayList(FunctionDefinition),
    };

    pub const EnumDeclaration = struct {
        pub const Member = struct {
            name: []const u8,
            value: ?ast.Expression = null,
        };
        pos: utils.Position,
        is_pub: bool,
        variables: std.ArrayList(VariableDefinition),
        subtypes: std.ArrayList(Subtype),
        name: []const u8,
        members: std.ArrayList(Member),
        methods: std.ArrayList(FunctionDefinition),
    };

    pub const While = struct {
        pos: utils.Position,
        condition: ast.Expression,
        capture: ?utils.Capture,
        body: *const Statement,
    };

    pub const BindingTypeDeclaration = struct {
        pos: utils.Position,
        is_pub: bool,
        name: []const u8,
        type: utils.CompoundTypeTag,
    };

    pub const BindingFunctionDeclaration = struct {
        pos: utils.Position,
        is_pub: bool,
        name: []const u8,
        parameters: ParameterList,
        return_type: Type,

        pub fn getType(self: *const BindingFunctionDeclaration) Type {
            return .{
                .function = .{
                    .pos = self.pos,
                    .name = self.name,
                    .generic_parameters = .empty,
                    .parameters = self.parameters,
                    .return_type = &self.return_type,
                },
            };
        }
    };

    pub const VariableDefinition = struct {
        pos: utils.Position,
        is_pub: bool,
        binding: enum { is_mut, is_const, neither },
        variable_name: []const u8,
        type: Type,
        assigned_value: ast.Expression,
    };
};

pub const VariableSignature = struct {
    is_mut: bool,
    name: []const u8,
    type: Type,
};

pub const Type = union(enum) {
    const Reference = struct {
        pos: utils.Position,
        inner: *const Type,
        is_mut: bool,
    };

    const Slice = Reference;

    const Array = struct {
        pos: utils.Position,
        inner: *const Type,
        /// if size is `_`, type is an array of inferred size.
        /// if size is a valid expression, type is an array of specified size.
        size: *const Expression,
    };

    const ErrorUnion = struct {
        pos: utils.Position,
        success: *const Type,
        failure: ?*const Type = null,
    };

    const Function = struct {
        pos: utils.Position,
        name: []const u8,
        parameters: ParameterList,
        generic_parameters: ParameterList,
        return_type: *const Type,
    };

    const Generic = struct {
        pos: utils.Position,
        lhs: *const Type,
        arguments: ArgumentList,
    };

    const Member = struct {
        pos: utils.Position,
        parent: *const Type,
        member_name: []const u8,
    };

    inferred: struct { pos: utils.Position },
    symbol: struct { pos: utils.Position, symbol: []const u8 },
    optional: struct { pos: utils.Position, inner: *const Type },
    slice: Slice,
    reference: Reference,
    array: Array,
    error_union: ErrorUnion,
    function: Function,
    generic: Generic,
    variadic: struct { pos: utils.Position },
    member: Member,

    pub inline fn getPosition(self: Type) utils.Position {
        return switch (self) {
            inline else => |some| some.pos,
        };
    }
};

pub const Subtype = union(utils.CompoundTypeTag) {
    @"struct": Statement.StructDeclaration,
    @"enum": Statement.EnumDeclaration,
    @"union": Statement.UnionDeclaration,
};

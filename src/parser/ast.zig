//! Declarative description of the AST.

const std = @import("std");
const utils = @import("utils");

const LexerToken = @import("Lexer").Token;

pub const ParameterList = []const VariableSignature;
pub const ArgumentList = []const Expression;
pub const RootNode = []const Statement;
pub const Block = []const Statement;

const ast = @This();

fn cloneArgumentList(args: ArgumentList, alloc: std.mem.Allocator) !ArgumentList {
    var new_args = try std.ArrayList(Expression).initCapacity(alloc, args.len);
    for (args) |arg| {
        try new_args.append(alloc, try arg.clone(alloc));
    }
    return new_args.toOwnedSlice(alloc);
}

fn cloneComparisonItemList(items: std.ArrayList(Expression.Comparison.Item), alloc: std.mem.Allocator) !std.ArrayList(Expression.Comparison.Item) {
    var new_items = try std.ArrayList(Expression.Comparison.Item).initCapacity(alloc, items.items.len);
    for (items.items) |item| {
        try new_items.append(alloc, .{ .op = item.op, .right = try item.right.clonePtr(alloc) });
    }
    return new_items;
}

fn cloneStringHashMapExpression(map: std.StringHashMap(Expression), alloc: std.mem.Allocator) !std.StringHashMap(Expression) {
    var new_map = std.StringHashMap(Expression).init(alloc);
    var it = map.iterator();
    while (it.next()) |entry| {
        try new_map.put(try alloc.dupe(u8, entry.key_ptr.*), try entry.value_ptr.*.clone(alloc));
    }
    return new_map;
}

fn cloneExpressionArrayList(list: std.ArrayList(Expression), alloc: std.mem.Allocator) !std.ArrayList(Expression) {
    var new_list = try std.ArrayList(Expression).initCapacity(alloc, list.items.len);
    for (list.items) |item| {
        try new_list.append(alloc, try item.clone(alloc));
    }
    return new_list;
}

fn cloneMatchCaseList(list: []const Expression.Match.Case, alloc: std.mem.Allocator) ![]const Expression.Match.Case {
    var new_list = try std.ArrayList(Expression.Match.Case).initCapacity(alloc, list.len);
    for (list) |item| {
        const new_case: Expression.Match.Case = .{
            .pos = try item.pos.clone(alloc),
            .condition = switch (item.condition) {
                .opts => |opts| .{ .opts = try cloneExpressionSlice(opts, alloc) },
                .@"else" => .@"else",
            },
            .result = try item.result.clone(alloc),
        };
        try new_list.append(alloc, new_case);
    }
    return new_list.toOwnedSlice(alloc);
}

fn cloneExpressionSlice(list: []const Expression, alloc: std.mem.Allocator) ![]const Expression {
    var new_list = try std.ArrayList(Expression).initCapacity(alloc, list.len);
    for (list) |item| {
        try new_list.append(alloc, try item.clone(alloc));
    }
    return new_list.toOwnedSlice(alloc);
}

fn cloneParameterList(params: ParameterList, alloc: std.mem.Allocator) !ParameterList {
    var new_params = try std.ArrayList(VariableSignature).initCapacity(alloc, params.len);
    for (params) |param| {
        try new_params.append(alloc, try param.clone(alloc));
    }
    return new_params.toOwnedSlice(alloc);
}

pub fn cloneSlice(comptime T: type, list: []const T, alloc: std.mem.Allocator) std.mem.Allocator.Error![]const T {
    const new_list = try alloc.alloc(T, list.len);
    for (list, 0..) |item, i| new_list[i] = try item.clone(alloc);
    return new_list;
}

fn cloneStringSliceSlice(slice_of_slices: []const []const u8, alloc: std.mem.Allocator) ![]const []const u8 {
    var new_list = try std.ArrayList([]const u8).initCapacity(alloc, slice_of_slices.len);
    for (slice_of_slices) |s| try new_list.append(alloc, try alloc.dupe(u8, s));
    return new_list.toOwnedSlice(alloc);
}

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
    ident: struct { pos: utils.Position, payload: []const u8 },
    string: struct { pos: utils.Position, payload: []const u8 },
    char: struct { pos: utils.Position, payload: u8 },
    int: struct { pos: utils.Position, payload: u64 },
    float: struct { pos: utils.Position, payload: f64 },

    @"if": If,
    array_instantiation: ArrayInstantiation,
    assignment: Assignment,
    binary: Binary,
    block: Expression.Block,
    call: Call,
    comparison: Comparison,
    dereference: Dereference,
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
                opts: []const Expression,
                @"else",
            };

            pos: utils.Position,
            condition: Condition,
            result: Statement,
        };

        pos: utils.Position,
        condition: *const Expression,
        cases: []const Case,
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

    pub const Dereference = struct {
        pos: utils.Position,
        parent: *const Expression,
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

    pub const If = struct {
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

    fn clonePtr(self: Expression, alloc: std.mem.Allocator) std.mem.Allocator.Error!*Expression {
        const ret = try alloc.create(Expression);
        ret.* = try self.clone(alloc);
        return ret;
    }

    pub fn clone(self: Expression, alloc: std.mem.Allocator) std.mem.Allocator.Error!Expression {
        return switch (self) {
            .bad_node => |n| .{ .bad_node = .{ .pos = try n.pos.clone(alloc) } },
            .ident => |n| .{ .ident = .{ .pos = try n.pos.clone(alloc), .payload = try alloc.dupe(u8, n.payload) } },
            .string => |n| .{ .string = .{ .pos = try n.pos.clone(alloc), .payload = try alloc.dupe(u8, n.payload) } },
            .char => |n| .{ .char = .{ .pos = try n.pos.clone(alloc), .payload = n.payload } },
            .int => |n| .{ .int = .{ .pos = try n.pos.clone(alloc), .payload = n.payload } },
            .float => |n| .{ .float = .{ .pos = try n.pos.clone(alloc), .payload = n.payload } },
            .block => |n| .{ .block = .{ .pos = try n.pos.clone(alloc), .block = try cloneSlice(Statement, n.block, alloc) } },
            .generic => |n| .{ .generic = .{ .pos = try n.pos.clone(alloc), .lhs = try n.lhs.clonePtr(alloc), .arguments = try cloneArgumentList(n.arguments, alloc) } },
            .binary => |n| .{ .binary = .{ .pos = try n.pos.clone(alloc), .lhs = try n.lhs.clonePtr(alloc), .op = n.op, .rhs = try n.rhs.clonePtr(alloc) } },
            .comparison => |n| .{
                .comparison = .{
                    .pos = try n.pos.clone(alloc),
                    .left = try n.left.clonePtr(alloc),
                    .comparisons = try cloneComparisonItemList(n.comparisons, alloc),
                },
            },
            .member => |n| .{ .member = .{ .pos = try n.pos.clone(alloc), .parent = try n.parent.clonePtr(alloc), .member_name = try alloc.dupe(u8, n.member_name) } },
            .dereference => |n| .{ .dereference = .{ .pos = try n.pos.clone(alloc), .parent = try n.parent.clonePtr(alloc) } },
            .call => |n| .{ .call = .{ .pos = try n.pos.clone(alloc), .callee = try n.callee.clonePtr(alloc), .args = try cloneArgumentList(n.args, alloc) } },
            .prefix => |n| .{ .prefix = .{ .pos = try n.pos.clone(alloc), .op = n.op, .rhs = try n.rhs.clonePtr(alloc) } },
            .assignment => |n| .{ .assignment = .{ .pos = try n.pos.clone(alloc), .assignee = try n.assignee.clonePtr(alloc), .op = n.op, .value = try n.value.clonePtr(alloc) } },
            .struct_instantiation => |n| .{
                .struct_instantiation = .{
                    .pos = try n.pos.clone(alloc),
                    .type_expr = try n.type_expr.clonePtr(alloc),
                    .members = try cloneStringHashMapExpression(n.members, alloc),
                },
            },
            .array_instantiation => |n| .{
                .array_instantiation = .{
                    .pos = try n.pos.clone(alloc),
                    .length = try n.length.clonePtr(alloc),
                    .type = try n.type.clone(alloc),
                    .contents = try cloneExpressionArrayList(n.contents, alloc),
                },
            },
            .range => |n| .{ .range = .{ .pos = try n.pos.clone(alloc), .start = try n.start.clonePtr(alloc), .end = if (n.end) |e| try e.clonePtr(alloc) else null, .inclusive = n.inclusive } },
            .reference => |n| .{ .reference = .{ .pos = try n.pos.clone(alloc), .inner = try n.inner.clonePtr(alloc), .is_mut = n.is_mut } },
            .@"if" => |n| .{
                .@"if" = .{
                    .pos = try n.pos.clone(alloc),
                    .condition = try n.condition.clonePtr(alloc),
                    .capture = if (n.capture) |c| try c.clone(alloc) else null,
                    .body = try n.body.clonePtr(alloc),
                    .@"else" = if (n.@"else") |e| try e.clonePtr(alloc) else null,
                },
            },
            .index => |n| .{
                .index = .{
                    .pos = try n.pos.clone(alloc),
                    .lhs = try n.lhs.clonePtr(alloc),
                    .index = try n.index.clonePtr(alloc),
                },
            },
            .slice => |n| .{
                .slice = .{
                    .pos = try n.pos.clone(alloc),
                    .lhs = try n.lhs.clonePtr(alloc),
                    .start = if (n.start) |s| try s.clonePtr(alloc) else null,
                    .end = if (n.end) |e| try e.clonePtr(alloc) else null,
                    .inclusive = n.inclusive,
                },
            },
            .match => |n| .{
                .match = .{
                    .pos = try n.pos.clone(alloc),
                    .condition = try n.condition.clonePtr(alloc),
                    .cases = try cloneMatchCaseList(n.cases, alloc),
                },
            },
            .type => |n| .{ .type = try n.clone(alloc) },
            .@"try" => |n| .{ .@"try" = .{ .pos = try n.pos.clone(alloc), .@"try" = try n.@"try".clonePtr(alloc) } },
            .@"catch" => |n| .{ .@"catch" = .{ .pos = try n.pos.clone(alloc), .lhs = try n.lhs.clonePtr(alloc), .rhs = try n.rhs.clonePtr(alloc) } },
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
        module_name: []const []const u8,
        alias: ?[]const u8,
    };

    pub const Return = struct {
        pos: utils.Position,
        @"return": ?ast.Expression,
    };

    fn clonePtr(self: Statement, alloc: std.mem.Allocator) std.mem.Allocator.Error!*Statement {
        const ret = try alloc.create(Statement);
        ret.* = try self.clone(alloc);
        return ret;
    }

    pub fn clone(self: Statement, alloc: std.mem.Allocator) std.mem.Allocator.Error!Statement {
        return switch (self) {
            .@"break" => .@"break",
            .@"continue" => .@"continue",
            .expression => |n| .{ .expression = try n.clone(alloc) },
            .block_eval => |n| .{ .block_eval = try n.clone(alloc) },
            .@"defer" => |n| .{ .@"defer" = .{ .pos = try n.pos.clone(alloc), .stmt = try n.stmt.clonePtr(alloc) } },
            .@"for" => |n| .{
                .@"for" = .{
                    .pos = try n.pos.clone(alloc),
                    .iterator = try n.iterator.clone(alloc),
                    .capture = if (n.capture) |c| try c.clone(alloc) else null,
                    .body = try n.body.clonePtr(alloc),
                },
            },
            .@"if" => |n| .{
                .@"if" = .{
                    .pos = try n.pos.clone(alloc),
                    .condition = try n.condition.clone(alloc),
                    .capture = if (n.capture) |c| try c.clone(alloc) else null,
                    .body = try n.body.clonePtr(alloc),
                    .@"else" = if (n.@"else") |e| try e.clonePtr(alloc) else null,
                },
            },
            .@"return" => |n| .{ .@"return" = .{ .pos = try n.pos.clone(alloc), .@"return" = if (n.@"return") |r| try r.clone(alloc) else null } },
            .@"while" => |n| .{
                .@"while" = .{
                    .pos = try n.pos.clone(alloc),
                    .condition = try n.condition.clone(alloc),
                    .capture = if (n.capture) |c| try c.clone(alloc) else null,
                    .body = try n.body.clonePtr(alloc),
                },
            },
            .binding_function_declaration => |n| .{
                .binding_function_declaration = .{
                    .pos = try n.pos.clone(alloc),
                    .is_pub = n.is_pub,
                    .name = try alloc.dupe(u8, n.name),
                    .parameters = try cloneParameterList(n.parameters, alloc),
                    .return_type = try n.return_type.clone(alloc),
                },
            },
            .binding_type_declaration => |n| .{
                .binding_type_declaration = .{
                    .pos = try n.pos.clone(alloc),
                    .is_pub = n.is_pub,
                    .name = try alloc.dupe(u8, n.name),
                    .type = n.type,
                },
            },
            .block => |n| .{ .block = .{ .pos = try n.pos.clone(alloc), .block = try cloneSlice(ast.Statement, n.block, alloc) } },
            .enum_declaration => |n| .{ .enum_declaration = try n.clone(alloc) },
            .function_definition => |n| .{
                .function_definition = .{
                    .pos = try n.pos.clone(alloc),
                    .is_pub = n.is_pub,
                    .name = try alloc.dupe(u8, n.name),
                    .generic_parameters = try cloneParameterList(n.generic_parameters, alloc),
                    .parameters = try cloneParameterList(n.parameters, alloc),
                    .return_type = try n.return_type.clone(alloc),
                    .body = try cloneSlice(ast.Statement, n.body, alloc),
                },
            },
            .import => |n| .{
                .import = .{
                    .pos = try n.pos.clone(alloc),
                    .module_name = try cloneStringSliceSlice(n.module_name, alloc),
                    .alias = if (n.alias) |a| try alloc.dupe(u8, a) else null,
                },
            },
            .struct_declaration => |n| .{ .struct_declaration = try n.clone(alloc) },
            .union_declaration => |n| .{ .union_declaration = try n.clone(alloc) },
            .variable_definition => |n| .{
                .variable_definition = .{
                    .pos = try n.pos.clone(alloc),
                    .is_pub = n.is_pub,
                    .binding = n.binding,
                    .variable_name = try alloc.dupe(u8, n.variable_name),
                    .type = try n.type.clone(alloc),
                    .assigned_value = try n.assigned_value.clone(alloc),
                },
            },
        };
    }

    fn CompoundTypeDeclaration(comptime T: enum { @"struct", @"union" }) type {
        return struct {
            const Member = struct {
                name: []const u8,
                type: if (T == .@"struct") Type else ?Type,
            };
            pos: utils.Position,
            is_pub: bool,
            name: []const u8,
            generic_types: ParameterList,
            variables: std.ArrayList(VariableDefinition),
            subtypes: std.ArrayList(Subtype),
            members: std.ArrayList(Member),
            methods: std.ArrayList(FunctionDefinition),

            pub fn clone(self: *const CompoundTypeDeclaration(T), alloc: std.mem.Allocator) !CompoundTypeDeclaration(T) {
                var new_generic_types = try alloc.alloc(VariableSignature, self.generic_types.len);
                for (self.generic_types, 0..) |p, i| new_generic_types[i] = try p.clone(alloc);

                return .{
                    .pos = self.pos,
                    .is_pub = self.is_pub,
                    .name = try alloc.dupe(u8, self.name),
                    .generic_types = new_generic_types,
                    .variables = try self.variables.clone(alloc),
                    .subtypes = try self.subtypes.clone(alloc),
                    .members = try self.members.clone(alloc),
                    .methods = try self.methods.clone(alloc),
                };
            }
        };
    }

    pub const StructDeclaration = CompoundTypeDeclaration(.@"struct");
    pub const UnionDeclaration = CompoundTypeDeclaration(.@"union");

    pub const EnumDeclaration = struct {
        pub const Member = struct {
            name: []const u8,
            value: ?ast.Expression = null,

            pub fn clone(self: Member, alloc: std.mem.Allocator) !Member {
                return .{
                    .name = try alloc.dupe(u8, self.name),
                    .value = self.value,
                };
            }
        };

        pos: utils.Position,
        is_pub: bool,
        name: []const u8,
        variables: std.ArrayList(VariableDefinition),
        subtypes: std.ArrayList(Subtype),
        members: std.ArrayList(Member),
        methods: std.ArrayList(FunctionDefinition),

        pub fn clone(self: *const EnumDeclaration, alloc: std.mem.Allocator) !EnumDeclaration {
            var new_members: std.ArrayList(Member) = try .initCapacity(alloc, self.members.items.len);
            for (self.members.items) |*m| new_members.appendAssumeCapacity(try m.clone(alloc));

            return .{
                .pos = self.pos,
                .is_pub = self.is_pub,
                .name = try alloc.dupe(u8, self.name),
                .variables = try self.variables.clone(alloc),
                .subtypes = try self.subtypes.clone(alloc),
                .members = new_members,
                .methods = try self.methods.clone(alloc),
            };
        }
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
                    .generic_parameters = &.{},
                    .parameters = self.parameters,
                    .return_type = &self.return_type,
                },
            };
        }
    };

    pub const VariableDefinition = struct {
        pos: utils.Position,
        is_pub: bool,
        binding: utils.Binding,
        variable_name: []const u8,
        type: Type,
        assigned_value: ast.Expression,
    };
};

pub const VariableSignature = struct {
    is_mut: bool,
    name: []const u8,
    type: Type,

    pub fn clone(self: VariableSignature, alloc: std.mem.Allocator) !VariableSignature {
        return .{
            .is_mut = self.is_mut,
            .name = try alloc.dupe(u8, self.name),
            .type = self.type,
        };
    }
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

    fn clonePtr(self: Type, alloc: std.mem.Allocator) std.mem.Allocator.Error!*Type {
        const ret = try alloc.create(Type);
        ret.* = try self.clone(alloc);
        return ret;
    }

    pub fn clone(self: Type, alloc: std.mem.Allocator) !Type {
        return switch (self) {
            .inferred => |n| .{ .inferred = .{ .pos = try n.pos.clone(alloc) } },
            .symbol => |n| .{ .symbol = .{ .pos = try n.pos.clone(alloc), .symbol = try alloc.dupe(u8, n.symbol) } },
            .optional => |n| .{ .optional = .{ .pos = try n.pos.clone(alloc), .inner = try n.inner.clonePtr(alloc) } },
            .slice => |n| .{ .slice = .{ .pos = try n.pos.clone(alloc), .inner = try n.inner.clonePtr(alloc), .is_mut = n.is_mut } },
            .reference => |n| .{ .reference = .{ .pos = try n.pos.clone(alloc), .inner = try n.inner.clonePtr(alloc), .is_mut = n.is_mut } },
            .array => |n| .{ .array = .{ .pos = try n.pos.clone(alloc), .inner = try n.inner.clonePtr(alloc), .size = try n.size.clonePtr(alloc) } },
            .error_union => |n| .{ .error_union = .{ .pos = try n.pos.clone(alloc), .success = try n.success.clonePtr(alloc), .failure = if (n.failure) |f| try f.clonePtr(alloc) else null } },
            .function => |n| .{
                .function = .{
                    .pos = try n.pos.clone(alloc),
                    .name = try alloc.dupe(u8, n.name),
                    .parameters = try cloneParameterList(n.parameters, alloc),
                    .generic_parameters = try cloneParameterList(n.generic_parameters, alloc),
                    .return_type = try n.return_type.clonePtr(alloc),
                },
            },
            .generic => |n| .{ .generic = .{ .pos = try n.pos.clone(alloc), .lhs = try n.lhs.clonePtr(alloc), .arguments = try cloneArgumentList(n.arguments, alloc) } },
            .variadic => |n| .{ .variadic = .{ .pos = try n.pos.clone(alloc) } },
            .member => |n| .{ .member = .{ .pos = try n.pos.clone(alloc), .parent = try n.parent.clonePtr(alloc), .member_name = try alloc.dupe(u8, n.member_name) } },
        };
    }
};

pub const Subtype = union(utils.CompoundTypeTag) {
    @"struct": Statement.StructDeclaration,
    @"enum": Statement.EnumDeclaration,
    @"union": Statement.UnionDeclaration,
};

//! Declarative description of the AST.

const std = @import("std");
const utils = @import("utils");

const LexerToken = @import("Lexer").Token;

pub const ParameterList = []const VariableSignature;
pub const ArgumentList = []const Expression;
pub const RootNode = []const Statement;
pub const Block = []const Statement;

const ast = @This();

pub fn cloneSlice(comptime T: type, list: []const T, alloc: std.mem.Allocator) ![]T {
    const new_list = try alloc.alloc(T, list.len);
    for (list, 0..) |item, i| new_list[i] = try item.clone(alloc);
    return new_list;
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

            fn clone(self: Case, alloc: std.mem.Allocator) !Case {
                return .{
                    .pos = try self.pos.clone(alloc),
                    .condition = switch (self.condition) {
                        .opts => |opts| .{ .opts = try cloneSlice(ast.Expression, opts, alloc) },
                        .@"else" => .@"else",
                    },
                    .result = try self.result.clone(alloc),
                };
            }
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

            pub fn clone(self: Item, alloc: std.mem.Allocator) !Item {
                return .{ .op = self.op, .right = try self.right.clonePtr(alloc) };
            }
        };

        pos: utils.Position,
        left: *const Expression,
        comparisons: []const Item,
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
        contents: []const Expression,
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

    fn clonePtr(self: Expression, alloc: std.mem.Allocator) !*Expression {
        const ret = try alloc.create(Expression);
        ret.* = try self.clone(alloc);
        return ret;
    }

    pub fn clone(self: Expression, alloc: std.mem.Allocator) std.mem.Allocator.Error!Expression {
        return switch (self) {
            inline .ident, .string => |s, t| @unionInit(Expression, @tagName(t), .{
                .pos = try s.pos.clone(alloc),
                .payload = try alloc.dupe(u8, s.payload),
            }),
            inline .int, .float, .char => |n, t| @unionInit(Expression, @tagName(t), .{
                .pos = try n.pos.clone(alloc),
                .payload = n.payload,
            }),
            .bad_node => |bn| .{ .bad_node = .{ .pos = try bn.pos.clone(alloc) } },
            .block => |block| .{
                .block = .{
                    .pos = try block.pos.clone(alloc),
                    .block = try cloneSlice(Statement, block.block, alloc),
                },
            },
            .generic => |generic| .{
                .generic = .{
                    .pos = try generic.pos.clone(alloc),
                    .lhs = try generic.lhs.clonePtr(alloc),
                    .arguments = try cloneSlice(ast.Expression, generic.arguments, alloc),
                },
            },
            .binary => |binary| .{
                .binary = .{
                    .pos = try binary.pos.clone(alloc),
                    .lhs = try binary.lhs.clonePtr(alloc),
                    .op = binary.op,
                    .rhs = try binary.rhs.clonePtr(alloc),
                },
            },
            .comparison => |comparison| .{
                .comparison = .{
                    .pos = try comparison.pos.clone(alloc),
                    .left = try comparison.left.clonePtr(alloc),
                    .comparisons = try cloneSlice(Comparison.Item, comparison.comparisons, alloc),
                },
            },
            .member => |member| .{
                .member = .{
                    .pos = try member.pos.clone(alloc),
                    .parent = try member.parent.clonePtr(alloc),
                    .member_name = try alloc.dupe(u8, member.member_name),
                },
            },
            .dereference => |dereference| .{
                .dereference = .{
                    .pos = try dereference.pos.clone(alloc),
                    .parent = try dereference.parent.clonePtr(alloc),
                },
            },
            .call => |call| .{
                .call = .{
                    .pos = try call.pos.clone(alloc),
                    .callee = try call.callee.clonePtr(alloc),
                    .args = try cloneSlice(ast.Expression, call.args, alloc),
                },
            },
            .prefix => |prefix| .{
                .prefix = .{
                    .pos = try prefix.pos.clone(alloc),
                    .op = prefix.op,
                    .rhs = try prefix.rhs.clonePtr(alloc),
                },
            },
            .assignment => |assignment| .{
                .assignment = .{
                    .pos = try assignment.pos.clone(alloc),
                    .assignee = try assignment.assignee.clonePtr(alloc),
                    .op = assignment.op,
                    .value = try assignment.value.clonePtr(alloc),
                },
            },
            .struct_instantiation => |si| .{
                .struct_instantiation = .{
                    .pos = try si.pos.clone(alloc),
                    .type_expr = try si.type_expr.clonePtr(alloc),
                    .members = b: {
                        var new_map: std.StringHashMap(Expression) = .init(alloc);
                        var it = si.members.iterator();
                        while (it.next()) |entry| try new_map.put(
                            try alloc.dupe(u8, entry.key_ptr.*),
                            try entry.value_ptr.*.clone(alloc),
                        );
                        break :b new_map;
                    },
                },
            },
            .array_instantiation => |ai| .{
                .array_instantiation = .{
                    .pos = try ai.pos.clone(alloc),
                    .length = try ai.length.clonePtr(alloc),
                    .type = try ai.type.clone(alloc),
                    .contents = try cloneSlice(ast.Expression, ai.contents, alloc),
                },
            },
            .range => |range| .{
                .range = .{
                    .pos = try range.pos.clone(alloc),
                    .start = try range.start.clonePtr(alloc),
                    .end = if (range.end) |e| try e.clonePtr(alloc) else null,
                    .inclusive = range.inclusive,
                },
            },
            .reference => |reference| .{
                .reference = .{
                    .pos = try reference.pos.clone(alloc),
                    .inner = try reference.inner.clonePtr(alloc),
                    .is_mut = reference.is_mut,
                },
            },
            .@"if" => |@"if"| .{
                .@"if" = .{
                    .pos = try @"if".pos.clone(alloc),
                    .condition = try @"if".condition.clonePtr(alloc),
                    .capture = if (@"if".capture) |c| try c.clone(alloc) else null,
                    .body = try @"if".body.clonePtr(alloc),
                    .@"else" = if (@"if".@"else") |e| try e.clonePtr(alloc) else null,
                },
            },
            .index => |index| .{
                .index = .{
                    .pos = try index.pos.clone(alloc),
                    .lhs = try index.lhs.clonePtr(alloc),
                    .index = try index.index.clonePtr(alloc),
                },
            },
            .slice => |slice| .{
                .slice = .{
                    .pos = try slice.pos.clone(alloc),
                    .lhs = try slice.lhs.clonePtr(alloc),
                    .start = if (slice.start) |s| try s.clonePtr(alloc) else null,
                    .end = if (slice.end) |e| try e.clonePtr(alloc) else null,
                    .inclusive = slice.inclusive,
                },
            },
            .match => |match| .{
                .match = .{
                    .pos = try match.pos.clone(alloc),
                    .condition = try match.condition.clonePtr(alloc),
                    .cases = try cloneSlice(Expression.Match.Case, match.cases, alloc),
                },
            },
            .type => |t| .{ .type = try t.clone(alloc) },
            .@"try" => |@"try"| .{
                .@"try" = .{
                    .pos = try @"try".pos.clone(alloc),
                    .@"try" = try @"try".@"try".clonePtr(alloc),
                },
            },
            .@"catch" => |@"catch"| .{
                .@"catch" = .{
                    .pos = try @"catch".pos.clone(alloc),
                    .lhs = try @"catch".lhs.clonePtr(alloc),
                    .rhs = try @"catch".rhs.clonePtr(alloc),
                },
            },
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
    expression: Expression,
    function_definition: FunctionDefinition,
    import: Import,
    enum_declaration: EnumDeclaration,
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

        pub fn clone(self: FunctionDefinition, alloc: std.mem.Allocator) !FunctionDefinition {
            return .{
                .pos = try self.pos.clone(alloc),
                .is_pub = self.is_pub,
                .name = try alloc.dupe(u8, self.name),
                .generic_parameters = try cloneSlice(VariableSignature, self.generic_parameters, alloc),
                .parameters = try cloneSlice(VariableSignature, self.parameters, alloc),
                .return_type = try self.return_type.clone(alloc),
                .body = try cloneSlice(ast.Statement, self.body, alloc),
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

    fn clonePtr(self: Statement, alloc: std.mem.Allocator) !*Statement {
        const ret = try alloc.create(Statement);
        ret.* = try self.clone(alloc);
        return ret;
    }

    pub fn clone(self: Statement, alloc: std.mem.Allocator) std.mem.Allocator.Error!Statement {
        return switch (self) {
            .@"break" => .@"break",
            .@"continue" => .@"continue",
            .@"defer" => |@"defer"| .{
                .@"defer" = .{
                    .pos = try @"defer".pos.clone(alloc),
                    .stmt = try @"defer".stmt.clonePtr(alloc),
                },
            },
            .@"for" => |@"for"| .{
                .@"for" = .{
                    .pos = try @"for".pos.clone(alloc),
                    .iterator = try @"for".iterator.clone(alloc),
                    .capture = if (@"for".capture) |c| try c.clone(alloc) else null,
                    .body = try @"for".body.clonePtr(alloc),
                },
            },
            .@"if" => |@"if"| .{
                .@"if" = .{
                    .pos = try @"if".pos.clone(alloc),
                    .condition = try @"if".condition.clone(alloc),
                    .capture = if (@"if".capture) |c| try c.clone(alloc) else null,
                    .body = try @"if".body.clonePtr(alloc),
                    .@"else" = if (@"if".@"else") |e| try e.clonePtr(alloc) else null,
                },
            },
            .@"return" => |@"return"| .{
                .@"return" = .{
                    .pos = try @"return".pos.clone(alloc),
                    .@"return" = if (@"return".@"return") |r| try r.clone(alloc) else null,
                },
            },
            .@"while" => |@"while"| .{
                .@"while" = .{
                    .pos = try @"while".pos.clone(alloc),
                    .condition = try @"while".condition.clone(alloc),
                    .capture = if (@"while".capture) |c| try c.clone(alloc) else null,
                    .body = try @"while".body.clonePtr(alloc),
                },
            },
            .binding_function_declaration => |bfd| .{
                .binding_function_declaration = .{
                    .pos = try bfd.pos.clone(alloc),
                    .is_pub = bfd.is_pub,
                    .name = try alloc.dupe(u8, bfd.name),
                    .parameters = try cloneSlice(VariableSignature, bfd.parameters, alloc),
                    .return_type = try bfd.return_type.clone(alloc),
                },
            },
            .binding_type_declaration => |btd| .{
                .binding_type_declaration = .{
                    .pos = try btd.pos.clone(alloc),
                    .is_pub = btd.is_pub,
                    .name = try alloc.dupe(u8, btd.name),
                    .type = btd.type,
                },
            },
            .block => |block| .{
                .block = .{
                    .pos = try block.pos.clone(alloc),
                    .block = try cloneSlice(ast.Statement, block.block, alloc),
                },
            },
            .import => |import| .{
                .import = .{
                    .pos = try import.pos.clone(alloc),
                    .module_name = b: {
                        const mn = try alloc.alloc([]const u8, import.module_name.len);
                        for (import.module_name, 0..) |name, i| mn[i] = try alloc.dupe(u8, name);
                        break :b mn;
                    },
                    .alias = if (import.alias) |a| try alloc.dupe(u8, a) else null,
                },
            },
            inline else => |other, t| @unionInit(Statement, @tagName(t), try other.clone(alloc)),
        };
    }

    fn CompoundTypeDeclaration(comptime T: enum { @"struct", @"union" }) type {
        return struct {
            pub const Member = struct {
                name: []const u8,
                type: if (T == .@"struct") Type else ?Type,

                fn clone(self: Member, alloc: std.mem.Allocator) !Member {
                    return switch (T) {
                        .@"struct" => .{
                            .name = try alloc.dupe(u8, self.name),
                            .type = try self.type.clone(alloc),
                        },
                        .@"union" => .{
                            .name = try alloc.dupe(u8, self.name),
                            .type = if (self.type) |t| try t.clone(alloc) else null,
                        },
                    };
                }
            };
            pos: utils.Position,
            is_pub: bool,
            name: []const u8,
            generic_types: ParameterList,
            variables: []const VariableDefinition,
            subtypes: []const Subtype,
            members: []const Member,
            methods: []const FunctionDefinition,

            pub fn clone(self: *const CompoundTypeDeclaration(T), alloc: std.mem.Allocator) !CompoundTypeDeclaration(T) {
                return .{
                    .pos = self.pos,
                    .is_pub = self.is_pub,
                    .name = try alloc.dupe(u8, self.name),
                    .generic_types = try cloneSlice(VariableSignature, self.generic_types, alloc),
                    .variables = try cloneSlice(VariableDefinition, self.variables, alloc),
                    .subtypes = try cloneSlice(Subtype, self.subtypes, alloc),
                    .members = try cloneSlice(Member, self.members, alloc),
                    .methods = try cloneSlice(FunctionDefinition, self.methods, alloc),
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
                    .value = if (self.value) |v| try v.clone(alloc) else null,
                };
            }
        };

        pos: utils.Position,
        is_pub: bool,
        name: []const u8,
        variables: []const VariableDefinition,
        subtypes: []const Subtype,
        members: []const Member,
        methods: []const FunctionDefinition,

        pub fn clone(self: *const EnumDeclaration, alloc: std.mem.Allocator) !EnumDeclaration {
            return .{
                .pos = self.pos,
                .is_pub = self.is_pub,
                .name = try alloc.dupe(u8, self.name),
                .variables = try cloneSlice(VariableDefinition, self.variables, alloc),
                .subtypes = try cloneSlice(Subtype, self.subtypes, alloc),
                .members = try cloneSlice(Member, self.members, alloc),
                .methods = try cloneSlice(FunctionDefinition, self.methods, alloc),
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

        pub fn clone(self: VariableDefinition, alloc: std.mem.Allocator) !VariableDefinition {
            return .{
                .pos = try self.pos.clone(alloc),
                .is_pub = self.is_pub,
                .binding = self.binding,
                .variable_name = try alloc.dupe(u8, self.variable_name),
                .type = try self.type.clone(alloc),
                .assigned_value = try self.assigned_value.clone(alloc),
            };
        }
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

    fn clonePtr(self: Type, alloc: std.mem.Allocator) !*Type {
        const ret = try alloc.create(Type);
        ret.* = try self.clone(alloc);
        return ret;
    }

    pub fn clone(self: Type, alloc: std.mem.Allocator) std.mem.Allocator.Error!Type {
        return switch (self) {
            .inferred => |n| .{ .inferred = .{ .pos = try n.pos.clone(alloc) } },
            .symbol => |n| .{
                .symbol = .{
                    .pos = try n.pos.clone(alloc),
                    .symbol = try alloc.dupe(u8, n.symbol),
                },
            },
            .optional => |n| .{
                .optional = .{
                    .pos = try n.pos.clone(alloc),
                    .inner = try n.inner.clonePtr(alloc),
                },
            },
            inline .reference, .slice => |n, t| @unionInit(Type, @tagName(t), .{
                .slice = .{
                    .pos = try n.pos.clone(alloc),
                    .inner = try n.inner.clonePtr(alloc),
                    .is_mut = n.is_mut,
                },
            }),
            .array => |n| .{
                .array = .{
                    .pos = try n.pos.clone(alloc),
                    .inner = try n.inner.clonePtr(alloc),
                    .size = try n.size.clonePtr(alloc),
                },
            },
            .error_union => |n| .{
                .error_union = .{
                    .pos = try n.pos.clone(alloc),
                    .success = try n.success.clonePtr(alloc),
                    .failure = if (n.failure) |f| try f.clonePtr(alloc) else null,
                },
            },
            .function => |n| .{
                .function = .{
                    .pos = try n.pos.clone(alloc),
                    .name = try alloc.dupe(u8, n.name),
                    .parameters = try cloneSlice(VariableSignature, n.parameters, alloc),
                    .generic_parameters = try cloneSlice(VariableSignature, n.generic_parameters, alloc),
                    .return_type = try n.return_type.clonePtr(alloc),
                },
            },
            .generic => |n| .{
                .generic = .{
                    .pos = try n.pos.clone(alloc),
                    .lhs = try n.lhs.clonePtr(alloc),
                    .arguments = try cloneSlice(Expression, n.arguments, alloc),
                },
            },
            .variadic => |n| .{ .variadic = .{ .pos = try n.pos.clone(alloc) } },
            .member => |n| .{
                .member = .{
                    .pos = try n.pos.clone(alloc),
                    .parent = try n.parent.clonePtr(alloc),
                    .member_name = try alloc.dupe(u8, n.member_name),
                },
            },
        };
    }
};

pub const Subtype = union(utils.CompoundTypeTag) {
    @"struct": Statement.StructDeclaration,
    @"enum": Statement.EnumDeclaration,
    @"union": Statement.UnionDeclaration,

    pub fn clone(self: Subtype, alloc: std.mem.Allocator) !Subtype {
        return switch (self) {
            inline else => |s, t| switch (try @unionInit(Statement, @tagName(t) ++ "_declaration", s).clone(alloc)) {
                .enum_declaration => |ed| .{ .@"enum" = ed },
                .struct_declaration => |sd| .{ .@"struct" = sd },
                .union_declaration => |ud| .{ .@"union" = ud },
                else => unreachable,
            },
        };
    }
};

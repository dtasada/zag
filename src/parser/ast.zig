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

pub fn deinitSlice(comptime T: type, list: []const T, alloc: std.mem.Allocator) void {
    for (list) |i| i.deinit(alloc);
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
    type: struct { pos: utils.Position, payload: Type },
    @"try": struct { pos: utils.Position, payload: *const Expression },
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

            fn deinit(self: Case, alloc: std.mem.Allocator) void {
                self.pos.deinit(alloc);
                if (self.condition == .opts) deinitSlice(Expression, self.condition.opts, alloc);
                self.result.deinit(alloc);
            }
        };

        pos: utils.Position,
        condition: *const Expression,
        cases: []const Case,
    };

    pub const Block = struct { pos: utils.Position, payload: ast.Block };
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

            pub fn deinit(self: Item, alloc: std.mem.Allocator) void {
                self.right.deinitPtr(alloc);
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
        members: *std.StringHashMap(Expression),
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
                    .payload = try cloneSlice(Statement, block.payload, alloc),
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
                        const new_map = try alloc.create(std.StringHashMap(Expression));
                        new_map.* = .init(alloc);
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
            .type => |t| .{ .type = .{ .pos = try t.pos.clone(alloc), .payload = try t.payload.clone(alloc) } },
            .@"try" => |@"try"| .{
                .@"try" = .{
                    .pos = try @"try".pos.clone(alloc),
                    .payload = try @"try".payload.clonePtr(alloc),
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

    pub fn deinitPtr(self: *const Expression, alloc: std.mem.Allocator) void {
        self.deinit(alloc);
        alloc.destroy(self);
    }

    pub fn deinit(self: Expression, alloc: std.mem.Allocator) void {
        switch (self) {
            inline else => |s| s.pos.deinit(alloc),
        }

        switch (self) {
            .bad_node, .char, .int, .float => {},
            inline .ident, .string => |s| alloc.free(s.payload),
            .@"if" => |@"if"| {
                @"if".condition.deinitPtr(alloc);
                if (@"if".capture) |c| c.deinit(alloc);

                @"if".body.deinitPtr(alloc);
                if (@"if".@"else") |@"else"| @"else".deinitPtr(alloc);
            },
            .array_instantiation => |ai| {
                ai.length.deinitPtr(alloc);
                ai.type.deinit(alloc);
                deinitSlice(Expression, ai.contents, alloc);
            },
            .assignment => |a| {
                a.assignee.deinitPtr(alloc);
                a.value.deinitPtr(alloc);
            },
            .binary => |bin| {
                bin.lhs.deinitPtr(alloc);
                bin.rhs.deinitPtr(alloc);
            },
            .block => |block| deinitSlice(Statement, block.payload, alloc),
            .call => |call| {
                call.callee.deinitPtr(alloc);
                deinitSlice(Expression, call.args, alloc);
            },
            .comparison => |cmp| {
                cmp.left.deinitPtr(alloc);
                deinitSlice(Comparison.Item, cmp.comparisons, alloc);
            },
            .dereference => |deref| deref.parent.deinitPtr(alloc),
            .generic => |generic| {
                generic.lhs.deinitPtr(alloc);
                deinitSlice(Expression, generic.arguments, alloc);
            },
            .index => |index| {
                index.lhs.deinitPtr(alloc);
                index.index.deinitPtr(alloc);
            },
            .slice => |slice| {
                slice.lhs.deinitPtr(alloc);
                if (slice.start) |start| start.deinitPtr(alloc);
                if (slice.end) |end| end.deinitPtr(alloc);
            },
            .match => |match| {
                match.condition.deinitPtr(alloc);
                deinitSlice(Match.Case, match.cases, alloc);
            },
            .member => |member| {
                member.parent.deinitPtr(alloc);
                alloc.free(member.member_name);
            },
            .prefix => |prefix| prefix.rhs.deinitPtr(alloc),
            .range => |range| {
                range.start.deinitPtr(alloc);
                if (range.end) |end| end.deinitPtr(alloc);
            },
            .reference => |ref| ref.inner.deinitPtr(alloc),
            .struct_instantiation => |si| {
                si.type_expr.deinitPtr(alloc);
                var it = si.members.iterator();
                while (it.next()) |expr| expr.value_ptr.deinit(alloc);
                si.members.deinit();
                alloc.destroy(si.members);
            },
            .type => |t| t.payload.deinit(alloc),
            .@"try" => |t| t.payload.deinitPtr(alloc),
            .@"catch" => |c| {
                c.lhs.deinitPtr(alloc);
                c.rhs.deinitPtr(alloc);
            },
        }
    }
};

pub const Statement = union(enum) {
    @"break": struct { pos: utils.Position },
    @"continue": struct { pos: utils.Position },
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
    @"defer": struct { pos: utils.Position, payload: *const Statement },

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

        pub fn deinit(self: FunctionDefinition, alloc: std.mem.Allocator) void {
            self.pos.deinit(alloc);
            alloc.free(self.name);
            deinitSlice(VariableSignature, self.generic_parameters, alloc);
            deinitSlice(VariableSignature, self.parameters, alloc);
            self.return_type.deinit(alloc);
            deinitSlice(Statement, self.body, alloc);
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

                fn deinit(self: Member, alloc: std.mem.Allocator) void {
                    alloc.free(self.name);
                    switch (T) {
                        .@"struct" => self.type.deinit(alloc),
                        .@"union" => if (self.type) |t| t.deinit(alloc),
                    }
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

            fn deinit(self: CompoundTypeDeclaration(T), alloc: std.mem.Allocator) void {
                self.pos.deinit(alloc);
                alloc.free(self.name);
                deinitSlice(VariableSignature, self.generic_types, alloc);
                deinitSlice(VariableDefinition, self.variables, alloc);
                deinitSlice(Subtype, self.subtypes, alloc);
                deinitSlice(Member, self.members, alloc);
                deinitSlice(FunctionDefinition, self.methods, alloc);
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

            fn deinit(self: Member, alloc: std.mem.Allocator) void {
                alloc.free(self.name);
                if (self.value) |v| v.deinit(alloc);
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

        fn deinit(self: EnumDeclaration, alloc: std.mem.Allocator) void {
            self.pos.deinit(alloc);
            alloc.free(self.name);
            deinitSlice(VariableDefinition, self.variables, alloc);
            deinitSlice(Subtype, self.subtypes, alloc);
            deinitSlice(Member, self.members, alloc);
            deinitSlice(FunctionDefinition, self.methods, alloc);
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

        fn deinit(self: VariableDefinition, alloc: std.mem.Allocator) void {
            self.pos.deinit(alloc);
            alloc.free(self.variable_name);
            self.type.deinit(alloc);
            self.assigned_value.deinit(alloc);
        }
    };

    fn clonePtr(self: Statement, alloc: std.mem.Allocator) !*Statement {
        const ret = try alloc.create(Statement);
        ret.* = try self.clone(alloc);
        return ret;
    }

    pub fn clone(self: Statement, alloc: std.mem.Allocator) std.mem.Allocator.Error!Statement {
        return switch (self) {
            .@"break" => |b| .{ .@"break" = .{ .pos = b.pos } },
            .@"continue" => |c| .{ .@"continue" = .{ .pos = c.pos } },
            .@"defer" => |@"defer"| .{
                .@"defer" = .{
                    .pos = try @"defer".pos.clone(alloc),
                    .payload = try @"defer".payload.clonePtr(alloc),
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
                    .payload = try cloneSlice(ast.Statement, block.payload, alloc),
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

    fn deinitPtr(self: *const Statement, alloc: std.mem.Allocator) void {
        self.deinit(alloc);
        alloc.destroy(self);
    }

    pub fn deinit(self: Statement, alloc: std.mem.Allocator) void {
        switch (self) {
            .expression, .block_eval => {},
            inline else => |s| s.pos.deinit(alloc),
        }

        switch (self) {
            .@"break", .@"continue" => {},
            .@"for" => |@"for"| {
                @"for".body.deinitPtr(alloc);
                if (@"for".capture) |c| c.deinit(alloc);
                @"for".iterator.deinit(alloc);
            },
            .@"if" => |@"if"| {
                @"if".condition.deinit(alloc);
                if (@"if".capture) |c| c.deinit(alloc);
                @"if".body.deinitPtr(alloc);
                if (@"if".@"else") |e| e.deinit(alloc);
            },
            .@"return" => |@"return"| if (@"return".@"return") |r| r.deinit(alloc),
            .@"while" => |@"while"| {
                @"while".condition.deinit(alloc);
                while (@"while".capture) |c| c.deinit(alloc);
                @"while".body.deinitPtr(alloc);
            },
            .binding_function_declaration => |bfd| {
                alloc.free(bfd.name);
                deinitSlice(VariableSignature, bfd.parameters, alloc);
                bfd.return_type.deinit(alloc);
            },
            .binding_type_declaration => |btd| alloc.free(btd.name),
            .block => |block| deinitSlice(Statement, block.payload, alloc),
            .expression, .block_eval => |expr| expr.deinit(alloc),
            .function_definition => |fd| {
                alloc.free(fd.name);
                deinitSlice(VariableSignature, fd.generic_parameters, alloc);
                deinitSlice(VariableSignature, fd.parameters, alloc);
                fd.return_type.deinit(alloc);
                deinitSlice(Statement, fd.body, alloc);
            },
            .import => |import| {
                for (import.module_name) |s| alloc.free(s);
                alloc.free(import.module_name);
                if (import.alias) |alias| alloc.free(alias);
            },
            inline .struct_declaration, .union_declaration, .enum_declaration => |sd, t| {
                alloc.free(sd.name);
                if (t != .enum_declaration) deinitSlice(VariableSignature, sd.generic_types, alloc);
                deinitSlice(VariableDefinition, sd.variables, alloc);
                deinitSlice(Subtype, sd.subtypes, alloc);
                deinitSlice(@TypeOf(sd).Member, sd.members, alloc);
                deinitSlice(FunctionDefinition, sd.methods, alloc);
            },
            .@"defer" => |d| d.payload.deinitPtr(alloc),
            .variable_definition => |vd| {
                alloc.free(vd.variable_name);
                vd.type.deinit(alloc);
                vd.assigned_value.deinit(alloc);
            },
        }
    }
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

    pub fn deinit(self: VariableSignature, alloc: std.mem.Allocator) void {
        alloc.free(self.name);
        self.type.deinit(alloc);
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
    symbol: struct { pos: utils.Position, inner: []const u8 },
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
                    .inner = try alloc.dupe(u8, n.inner),
                },
            },
            .optional => |n| .{
                .optional = .{
                    .pos = try n.pos.clone(alloc),
                    .inner = try n.inner.clonePtr(alloc),
                },
            },
            inline .reference, .slice => |n, t| @unionInit(Type, @tagName(t), .{
                .pos = try n.pos.clone(alloc),
                .inner = try n.inner.clonePtr(alloc),
                .is_mut = n.is_mut,
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

    fn deinitPtr(self: *const Type, alloc: std.mem.Allocator) void {
        self.deinit(alloc);
        alloc.destroy(self);
    }

    pub fn deinit(self: Type, alloc: std.mem.Allocator) void {
        switch (self) {
            inline else => |s| s.pos.deinit(alloc),
        }

        switch (self) {
            .inferred, .variadic => {},
            .symbol => |s| alloc.free(s.inner),
            .optional => |opt| opt.inner.deinitPtr(alloc),
            .slice, .reference => |s| s.inner.deinitPtr(alloc),
            .array => |a| {
                a.inner.deinitPtr(alloc);
                a.size.deinitPtr(alloc);
            },
            .error_union => |eu| {
                eu.success.deinitPtr(alloc);
                if (eu.failure) |f| f.deinitPtr(alloc);
            },
            .function => |f| {
                alloc.free(f.name);
                deinitSlice(VariableSignature, f.parameters, alloc);
                deinitSlice(VariableSignature, f.generic_parameters, alloc);
                f.return_type.deinitPtr(alloc);
            },
            .generic => |g| {
                g.lhs.deinitPtr(alloc);
                deinitSlice(Expression, g.arguments, alloc);
            },
            .member => |m| {
                m.parent.deinitPtr(alloc);
                alloc.free(m.member_name);
            },
        }
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

    pub fn deinit(self: Subtype, alloc: std.mem.Allocator) void {
        switch (self) {
            inline else => |s| s.deinit(alloc),
        }
    }
};

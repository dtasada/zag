//! Declarative description of the AST.

const std = @import("std");
const utils = @import("utils");

const Token = @import("lexer").Token;

pub const Statement = @import("statement.zig").Statement;
pub const Expression = @import("expression.zig").Expression;
pub const Type = @import("type.zig").Type;

pub const ParameterList = []const VariableSignature;
pub const ArgumentList = []const Expression;
pub const RootNode = []const TopLevelStatement;
pub const Block = []const Statement;

pub const TopLevelStatement = union(enum) {
    binding_function_declaration: Statement.BindingFunctionDeclaration,
    binding_type_declaration: Statement.BindingTypeDeclaration,
    function_definition: Statement.FunctionDefinition,
    import: Statement.Import,
    enum_declaration: Statement.EnumDeclaration,
    struct_declaration: Statement.StructDeclaration,
    union_declaration: Statement.UnionDeclaration,
    variable_definition: Statement.VariableDefinition,

    pub fn deinit(self: TopLevelStatement, alloc: std.mem.Allocator) void {
        switch (self) {
            inline else => |s| s.deinit(alloc),
        }
    }
};

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

    pub fn fromLexerToken(t: Token) BinaryOperator {
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

    pub fn fromLexerToken(t: Token) AssignmentOperator {
        return std.meta.stringToEnum(AssignmentOperator, @tagName(std.meta.activeTag(t))) orelse
            @panic("called AssignmentOperator.fromLexerToken on Lexer.Token that is not an assignment operator");
    }
};

pub const PrefixOperator = enum {
    @"-",
    @"!",

    pub fn fromLexerToken(t: Token) PrefixOperator {
        return std.meta.stringToEnum(PrefixOperator, @tagName(std.meta.activeTag(t))) orelse
            @panic("called PrefixOperator.fromLexerToken on Lexer.Token that is not a prefix operator");
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
            .type = try self.type.clone(alloc),
        };
    }

    pub fn deinit(self: VariableSignature, alloc: std.mem.Allocator) void {
        alloc.free(self.name);
        self.type.deinit(alloc);
    }
};

pub const Subtype = union(utils.CompoundTypeTag) {
    @"struct": Statement.StructDeclaration,
    @"enum": Statement.EnumDeclaration,
    @"union": Statement.UnionDeclaration,

    pub fn clone(self: Subtype, alloc: std.mem.Allocator) std.mem.Allocator.Error!Subtype {
        return switch (self) {
            .@"struct" => |*s| .{ .@"struct" = try s.clone(alloc) },
            .@"enum" => |*e| .{ .@"enum" = try e.clone(alloc) },
            .@"union" => |*u| .{ .@"union" = try u.clone(alloc) },
        };
    }

    pub fn deinit(self: Subtype, alloc: std.mem.Allocator) void {
        switch (self) {
            inline else => |s| s.deinit(alloc),
        }
    }
};

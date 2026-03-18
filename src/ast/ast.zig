//! Declarative description of the AST.

const std = @import("std");
const utils = @import("utils");

const LexerToken = @import("Lexer").Token;

pub const Statement = @import("statement.zig").Statement;
pub const Expression = @import("expression.zig").Expression;
pub const Type = @import("type.zig").Type;

pub const ParameterList = []const VariableSignature;
pub const ArgumentList = []const Expression;
pub const RootNode = []const Statement;
pub const Block = []const Statement;

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

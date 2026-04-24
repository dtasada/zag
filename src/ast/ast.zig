//! Declarative description of the AST.

const std = @import("std");
const utils = @import("utils");

const Token = @import("lexer").Token;

pub const Statement = @import("statement.zig").Statement;
pub const Expression = @import("expression.zig").Expression;
pub const TopLevelStatement = @import("top_level_statement.zig").TopLevelStatement;
pub const Type = @import("type.zig").Type;

pub const ParameterList = []const ParameterGroup;
pub const ArgumentList = []const Expression;
pub const RootNode = []const TopLevelStatement;
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

    pub fn format(self: PrefixOperator, writer: *std.Io.Writer) std.Io.Writer.Error!void {
        try writer.writeAll(@tagName(self));
    }
};

pub const VariableSignature = struct {
    is_mut: bool,
    name: []const u8,
    type: Type,

    pub fn clone(self: VariableSignature, alloc: std.mem.Allocator) !VariableSignature {
        const name = try alloc.dupe(u8, self.name);
        errdefer alloc.free(name);

        const t = try self.type.clone(alloc);
        errdefer t.deinit(alloc);

        return .{
            .is_mut = self.is_mut,
            .name = name,
            .type = t,
        };
    }

    pub fn deinit(self: VariableSignature, alloc: std.mem.Allocator) void {
        alloc.free(self.name);
        self.type.deinit(alloc);
    }
};

pub const ParameterGroup = struct {
    names: []const []const u8,
    is_mut: []const bool,
    type: Type,

    pub fn clone(self: ParameterGroup, alloc: std.mem.Allocator) !ParameterGroup {
        const names = try alloc.alloc([]const u8, self.names.len);
        errdefer alloc.free(names);
        for (self.names, 0..) |n, i| {
            names[i] = try alloc.dupe(u8, n);
        }
        errdefer for (names) |n| alloc.free(n);

        const is_mut = try alloc.dupe(bool, self.is_mut);
        errdefer alloc.free(is_mut);

        const t = try self.type.clone(alloc);
        errdefer t.deinit(alloc);

        return .{
            .names = names,
            .is_mut = is_mut,
            .type = t,
        };
    }

    pub fn deinit(self: ParameterGroup, alloc: std.mem.Allocator) void {
        self.type.deinit(alloc);
        alloc.free(self.is_mut);
        for (self.names) |n| alloc.free(n);
        alloc.free(self.names);
    }
};

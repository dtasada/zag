const std = @import("std");
const utils = @import("utils.zig");

const Lexer = @import("Lexer.zig");
const Parser = @import("Parser.zig");

const BindingPower = Parser.BindingPower;
const ParserError = Parser.ParserError;

const ast = @import("ast.zig");

const Self = @This();

const NudHandler = *const fn (*Self, std.mem.Allocator) ParserError!ast.Type;
const LedHandler = *const fn (*Self, std.mem.Allocator, ast.Type, BindingPower) ParserError!ast.Type;

const NudLookup = std.AutoHashMap(Lexer.TokenKind, NudHandler);
const LedLookup = std.AutoHashMap(Lexer.TokenKind, LedHandler);
const BpLookup = std.AutoHashMap(Lexer.TokenKind, BindingPower);

parent_parser: *Parser,
bp_lookup: BpLookup,
nud_lookup: NudLookup,
led_lookup: LedLookup,

/// A token which has a NUD handler means it expects nothing to its left
/// Common examples of this type of token are prefix & unary expressions.
fn nud(self: *Self, kind: Lexer.TokenKind, nud_fn: NudHandler) !void {
    try self.bp_lookup.put(kind, .primary);
    try self.nud_lookup.put(kind, nud_fn);
}

/// Tokens which have an LED expect to be between or after some other expression
/// to their left. Examples of this type of handler include binary expressions and
/// all infix expressions. Postfix expressions also fall under the LED handler.
fn led(self: *Self, kind: Lexer.TokenKind, bp: BindingPower, led_fn: LedHandler) !void {
    try self.bp_lookup.put(kind, bp);
    try self.led_lookup.put(kind, led_fn);
}

pub fn init(alloc: std.mem.Allocator, parent_parser: *Parser) !Self {
    var self = Self{
        .parent_parser = parent_parser,
        .bp_lookup = .init(alloc),
        .nud_lookup = .init(alloc),
        .led_lookup = .init(alloc),
    };

    try self.nud(Lexer.Token.ident, parseSymbolType);
    try self.nud(Lexer.Token.open_bracket, parseArrayType);

    return self;
}

pub fn parseSymbolType(self: *Self, _: std.mem.Allocator) ParserError!ast.Type {
    std.debug.print("current token: {f}\n", .{self.parent_parser.currentToken()});
    return .{
        .symbol = try self.parent_parser.expect(
            self.parent_parser.advance(),
            Lexer.Token.ident,
            "type descriptor",
            "type name",
        ),
    };
}

pub fn parseArrayType(self: *Self, alloc: std.mem.Allocator) ParserError!ast.Type {
    const underlying_type = try alloc.create(ast.Type);
    underlying_type.* = try self.parseType(alloc, .default);

    _ = self.parent_parser.advance();
    try self.parent_parser.expect(
        self.parent_parser.currentToken(),
        Lexer.Token.close_bracket,
        "array type descriptor",
        "]",
    );

    return .{ .array = underlying_type };
}

pub fn parseType(self: *Self, alloc: std.mem.Allocator, bp: BindingPower) ParserError!ast.Type {
    std.debug.print("!!!!current token: {f}\n", .{self.parent_parser.input.tokens.items[self.parent_parser.pos]});
    std.debug.print("!!!!pos token: {}\n", .{self.parent_parser.pos});
    // first parse the NUD
    const token_kind = self.parent_parser.currentTokenKind();
    const nud_fn = try self.getHandler(.nud, token_kind);

    const lhs = try alloc.create(ast.Type);
    lhs.* = try nud_fn(self, alloc);

    // while we have a led and (current bp < bp of current token)
    // continue parsing lhs
    while (true) {
        const current_token_kind = self.parent_parser.currentTokenKind();
        const current_bp = self.bp_lookup.get(current_token_kind) orelse break;

        if (@intFromEnum(current_bp) <= @intFromEnum(bp)) {
            break;
        }

        const led_fn = try self.getHandler(.led, current_token_kind);

        lhs.* = try led_fn(self, alloc, lhs.*, try self.getHandler(.bp, self.parent_parser.currentTokenKind()));
    }

    return lhs.*;
}

inline fn getHandler(
    self: *const Self,
    comptime handler_type: enum { nud, led, bp },
    token: Lexer.TokenKind,
) ParserError!switch (handler_type) {
    .nud => NudHandler,
    .led => LedHandler,
    .bp => BindingPower,
} {
    if (switch (handler_type) {
        .nud => self.nud_lookup,
        .led => self.led_lookup,
        .bp => self.bp_lookup,
    }.get(token)) |handler| {
        return handler;
    } else {
        utils.print("TypeParser: {s} handler for '{}' does not exist.\n", .{ @tagName(handler_type), token }, .red);
        return error.HandlerDoesNotExist;
    }
}

const std = @import("std");
const utils = @import("utils");
const expression = @import("expressions.zig");

const Lexer = @import("Lexer");
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
    try self.nud(Lexer.Token.ampersand, parseReferenceType);
    try self.nud(Lexer.Token.question, parseOptionalType);
    try self.nud(Lexer.Token.bang, parseInferredErrorType);
    try self.nud(Lexer.Token.open_paren, parseGroupType);
    try self.led(Lexer.Token.bang, .logical, parseErrorType);
    return self;
}

pub fn parseType(self: *Self, alloc: std.mem.Allocator, bp: BindingPower) ParserError!ast.Type {
    // first parse the NUD
    const token_kind = self.parent_parser.currentTokenKind();
    const nud_fn = try self.getHandler(.nud, token_kind);

    var lhs = try nud_fn(self, alloc);

    // while we have a led and (current bp < bp of current token)
    // continue parsing lhs
    while (self.bp_lookup.get(self.parent_parser.currentTokenKind())) |current_bp| {
        if (@intFromEnum(current_bp) <= @intFromEnum(bp)) break;

        const led_fn = self.getHandler(.led, self.parent_parser.currentTokenKind()) catch |err|
            switch (err) {
                error.HandlerDoesNotExist => {
                    utils.print("Expected type, received other.", .{}, .red);
                    return err;
                },
                else => return err,
            };
        lhs = try led_fn(self, alloc, lhs, current_bp);
    }

    return lhs;
}

pub fn parseSymbolType(self: *Self, _: std.mem.Allocator) ParserError!ast.Type {
    const ident = try self.parent_parser.expect(
        self.parent_parser.advance(),
        Lexer.Token.ident,
        "type descriptor",
        "type name",
    );

    if (std.mem.eql(u8, ident, "Self")) {
        return .{
            .self_type = {},
        };
    }

    return .{ .symbol = ident };
}

pub fn parseReferenceType(self: *Self, alloc: std.mem.Allocator) ParserError!ast.Type {
    _ = self.parent_parser.advance(); // consume '&'

    const is_mut = self.parent_parser.currentTokenKind() == Lexer.Token.mut;
    if (is_mut) _ = self.parent_parser.advance(); // consume `mut`

    const inner = try alloc.create(ast.Type);
    inner.* = try parseType(self, alloc, .default);

    return .{
        .reference = .{
            .inner = inner,
            .is_mut = is_mut,
        },
    };
}

pub fn parseOptionalType(self: *Self, alloc: std.mem.Allocator) ParserError!ast.Type {
    _ = self.parent_parser.advance(); // consume '?'

    const inner = try alloc.create(ast.Type);
    inner.* = try parseType(self, alloc, .default);

    return .{ .optional = inner };
}

pub fn parseInferredErrorType(self: *Self, alloc: std.mem.Allocator) ParserError!ast.Type {
    _ = self.parent_parser.advance(); // consume '!'

    const success = try alloc.create(ast.Type);
    success.* = try parseType(self, alloc, .default);

    return .{ .error_union = .{ .success = success } };
}

pub fn parseErrorType(self: *Self, alloc: std.mem.Allocator, lhs: ast.Type, _: BindingPower) ParserError!ast.Type {
    _ = self.parent_parser.advance(); // consume '!'

    const @"error" = try alloc.create(ast.Type);
    @"error".* = lhs;

    const success = try alloc.create(ast.Type);
    success.* = try parseType(self, alloc, .default);

    return .{
        .error_union = .{
            .success = success,
            .@"error" = @"error",
        },
    };
}

pub fn parseArrayType(self: *Self, alloc: std.mem.Allocator) ParserError!ast.Type {
    _ = self.parent_parser.advance(); // consume '['

    var size: ?*ast.Expression = null;

    if (self.parent_parser.currentTokenKind() != Lexer.Token.close_bracket) {
        size = try alloc.create(ast.Expression);
        size.?.* = try expression.parse(self.parent_parser, .default);
    }

    try self.parent_parser.expect(
        self.parent_parser.advance(),
        Lexer.Token.close_bracket,
        "array type descriptor",
        "]",
    );

    const inner = try alloc.create(ast.Type);
    inner.* = try self.parseType(alloc, .default);

    return .{
        .array = .{
            .inner = inner,
            .size = size,
        },
    };
}

pub fn parseGroupType(self: *Self, alloc: std.mem.Allocator) ParserError!ast.Type {
    try self.parent_parser.expect(self.parent_parser.advance(), Lexer.Token.open_paren, "group expression", "(");
    const @"type" = try parseType(self, alloc, .default);
    try self.parent_parser.expect(self.parent_parser.advance(), Lexer.Token.close_paren, "group expression", ")");

    return @"type";
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
    } else return utils.printErr(
        error.HandlerDoesNotExist,
        "TypeParser: {s} handler for '{}' does not exist.\n",
        .{ @tagName(handler_type), token },
        .red,
    );
}

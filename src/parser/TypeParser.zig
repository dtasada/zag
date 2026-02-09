const std = @import("std");
const utils = @import("utils");
const expressions = @import("expressions.zig");

const Lexer = @import("Lexer");
const Parser = @import("Parser.zig");

const BindingPower = Parser.BindingPower;
const ParserError = Parser.ParserError;

const ast = @import("ast.zig");

const Self = @This();

const NudHandler = *const fn (*Self, std.mem.Allocator) ParserError!ast.Type;
const LedHandler = *const fn (*Self, std.mem.Allocator, ast.Type, BindingPower) ParserError!ast.Type;

parent_parser: *Parser,
bp_lookup: std.AutoHashMap(Lexer.TokenKind, BindingPower),
nud_lookup: std.AutoHashMap(Lexer.TokenKind, NudHandler),
led_lookup: std.AutoHashMap(Lexer.TokenKind, LedHandler),

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

    try self.nud(.ident, parseSymbolType);
    try self.nud(.@"[", parseArrayType);
    try self.nud(.@"&", parseReferenceType);
    try self.nud(.@"?", parseOptionalType);
    try self.nud(.@"!", parseInferredErrorType);
    try self.nud(.@"fn", parseFunctionType);
    try self.nud(.@"(", parseGroupType);
    try self.led(.@"!", .logical, parseErrorType);
    try self.led(.@"<", .call, parseGenericType);
    return self;
}

fn getBindingPower(self: *Self, token: Lexer.TokenKind) BindingPower {
    return self.bp_lookup.get(token) orelse .default;
}

pub fn parseType(self: *Self, alloc: std.mem.Allocator, precedence: BindingPower) ParserError!ast.Type {
    var token = self.parent_parser.currentToken();
    var left = if (self.nud_lookup.get(std.meta.activeTag(token))) |nud_handler|
        try nud_handler(self, alloc)
    else {
        const pos = self.parent_parser.currentPosition();
        _ = self.parent_parser.advance();
        return utils.printErr(
            error.UnexpectedToken,
            "Unexpected token '{s}' in type at {f}\n",
            .{ @tagName(std.meta.activeTag(token)), pos },
            .red,
        );
    };

    while (@intFromEnum(precedence) < @intFromEnum(self.getBindingPower(self.parent_parser.currentToken()))) {
        token = self.parent_parser.advance(); // consume operator
        if (self.led_lookup.get(std.meta.activeTag(token))) |led_handler| {
            left = try led_handler(self, alloc, left, self.getBindingPower(std.meta.activeTag(token)));
        } else {
            return left;
        }
    }

    return left;
}

pub fn parseGenericType(self: *Self, alloc: std.mem.Allocator, lhs: ast.Type, _: BindingPower) ParserError!ast.Type {
    // < is already consumed by parseType loop
    var args = try std.ArrayList(ast.Expression).initCapacity(alloc, 0);

    if (self.parent_parser.currentToken() != .@">") {
        while (true) {
            const arg = try expressions.parse(self.parent_parser, .relational, .{});
            try args.append(alloc, arg);

            if (self.parent_parser.currentToken() == .@">") break;
            try self.parent_parser.expect(self.parent_parser.advance(), .@",", "generic arguments", ",");
            if (self.parent_parser.currentToken() == .@">") break;
        }
    }
    _ = self.parent_parser.advance(); // consume >

    const ptr = try alloc.create(ast.Type);
    ptr.* = lhs;

    return .{
        .generic = .{
            .pos = lhs.getPosition(), // Simplification: use start pos
            .lhs = ptr,
            .arguments = args,
        },
    };
}

// ... existing handlers ...

pub fn parseSymbolType(self: *Self, _: std.mem.Allocator) ParserError!ast.Type {
    const position = self.parent_parser.currentPosition();
    const ident = try self.parent_parser.expect(
        self.parent_parser.advance(),
        .ident,
        "type descriptor",
        "type name",
    );

    return .{ .symbol = .{ .symbol = ident, .pos = position } };
}

pub fn parseReferenceType(self: *Self, alloc: std.mem.Allocator) ParserError!ast.Type {
    const position = self.parent_parser.currentPosition();
    _ = self.parent_parser.advance(); // consume '&'

    const is_mut = self.parent_parser.currentToken() == .mut;
    if (is_mut) _ = self.parent_parser.advance(); // consume `mut`

    const inner = try alloc.create(ast.Type);
    inner.* = try parseType(self, alloc, .default);

    return .{
        .reference = .{
            .pos = position,
            .inner = inner,
            .is_mut = is_mut,
        },
    };
}

pub fn parseOptionalType(self: *Self, alloc: std.mem.Allocator) ParserError!ast.Type {
    const position = self.parent_parser.currentPosition();
    _ = self.parent_parser.advance(); // consume '?'

    const inner = try alloc.create(ast.Type);
    inner.* = try parseType(self, alloc, .default);

    return .{ .optional = .{ .pos = position, .inner = inner } };
}

pub fn parseInferredErrorType(self: *Self, alloc: std.mem.Allocator) ParserError!ast.Type {
    const position = self.parent_parser.currentPosition();
    _ = self.parent_parser.advance(); // consume '!'

    const success = try alloc.create(ast.Type);
    success.* = try parseType(self, alloc, .default);

    return .{ .error_union = .{ .pos = position, .success = success } };
}

pub fn parseFunctionType(self: *Self, alloc: std.mem.Allocator) ParserError!ast.Type {
    const pos = self.parent_parser.currentPosition();
    _ = self.parent_parser.advance(); // consume "fn" keyword
    const generic_parameters: ast.ParameterList = switch (self.parent_parser.currentToken()) {
        .@"(" => .empty,
        .@"<" => try self.parent_parser.parseGenericParameters(),
        else => |other| return self.parent_parser.unexpectedToken("function type", "(' or '<", other),
    };
    const parameters = try self.parent_parser.parseParameters();
    const return_type = try alloc.create(ast.Type);
    return_type.* = parseType(self, alloc, .default) catch |err| switch (err) {
        error.HandlerDoesNotExist, error.UnexpectedToken => return utils.printErr(
            error.MissingReturnType,
            "Parser error: missing return type in function type at {f}.\n",
            .{self.parent_parser.currentPosition()},
            .red,
        ),
        else => return err,
    };

    return .{
        .function = .{
            .pos = pos,
            .name = "function_type",
            .parameters = parameters,
            .generic_parameters = generic_parameters,
            .return_type = return_type,
        },
    };
}

pub fn parseErrorType(self: *Self, alloc: std.mem.Allocator, lhs: ast.Type, _: BindingPower) ParserError!ast.Type {
    const position = self.parent_parser.currentPosition();
    _ = self.parent_parser.advance(); // consume '!'

    const failure = try alloc.create(ast.Type);
    failure.* = lhs;

    const success = try alloc.create(ast.Type);
    success.* = try parseType(self, alloc, .default);

    return .{
        .error_union = .{
            .pos = position,
            .success = success,
            .failure = failure,
        },
    };
}

pub fn parseArrayType(self: *Self, alloc: std.mem.Allocator) ParserError!ast.Type {
    const position = self.parent_parser.currentPosition();
    _ = self.parent_parser.advance(); // consume '['

    var size: ?*ast.Expression = null;

    if (self.parent_parser.currentToken() != .@"]") {
        size = try alloc.create(ast.Expression);
        size.?.* = try expressions.parse(self.parent_parser, .default, .{});
    }

    try self.parent_parser.expect(self.parent_parser.advance(), .@"]", "array type descriptor", "]");

    const is_mut: ?bool = if (size == null)
        if (self.parent_parser.currentToken() == .mut) b: {
            _ = self.parent_parser.advance();
            break :b true;
        } else false
    else
        null;

    const inner = try alloc.create(ast.Type);
    inner.* = try self.parseType(alloc, .default);

    return if (size) |s| .{
        .array = .{
            .pos = position,
            .inner = inner,
            .size = s,
        },
    } else .{
        .slice = .{
            .pos = position,
            .inner = inner,
            .is_mut = is_mut.?,
        },
    };
}

pub fn parseGroupType(self: *Self, alloc: std.mem.Allocator) ParserError!ast.Type {
    try self.parent_parser.expect(self.parent_parser.advance(), .@"(", "group expression", "(");
    const @"type" = try parseType(self, alloc, .default);
    try self.parent_parser.expect(self.parent_parser.advance(), .@")", "group expression", ")");

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

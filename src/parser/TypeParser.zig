const std = @import("std");
const utils = @import("utils");
const expressions = @import("expressions.zig");

const lexer = @import("lexer");
const parser = @import("parser.zig");

const BindingPower = parser.BindingPower;
const Error = parser.Error;

const ast = @import("ast");

const Self = @This();

const NudHandler = *const fn (*Self, std.mem.Allocator, std.Io) Error!ast.Type;
const LedHandler = *const fn (*Self, std.mem.Allocator, std.Io, ast.Type, BindingPower) Error!ast.Type;

parent_parser: *parser.Parser,
bp_lookup: std.EnumMap(lexer.TokenKind, BindingPower),
nud_lookup: std.EnumMap(lexer.TokenKind, NudHandler),
led_lookup: std.EnumMap(lexer.TokenKind, LedHandler),

/// A token which has a NUD handler means it expects nothing to its left
/// Common examples of this type of token are prefix & unary expressions.
fn nud(self: *Self, kind: lexer.TokenKind, nud_fn: NudHandler) !void {
    try self.bp_lookup.put(kind, .primary);
    try self.nud_lookup.put(kind, nud_fn);
}

/// Tokens which have an LED expect to be between or after some other expression
/// to their left. Examples of this type of handler include binary expressions and
/// all infix expressions. Postfix expressions also fall under the LED handler.
fn led(self: *Self, kind: lexer.TokenKind, bp: BindingPower, led_fn: LedHandler) !void {
    try self.bp_lookup.put(kind, bp);
    try self.led_lookup.put(kind, led_fn);
}

pub fn init(parent_parser: *parser.Parser) !Self {
    return .{
        .parent_parser = parent_parser,
        .bp_lookup = .init(.{ .@"!" = .logical, .@"<" = .call, .@"." = .member }),
        .nud_lookup = .init(.{
            .ident = parseSymbolType,
            .@"[" = parseArrayType,
            .@"&" = parseReferenceType,
            .@"?" = parseOptionalType,
            .@"fn" = parseFunctionType,
            .@"(" = parseGroupType,
            .@"..." = parseVariadic,
        }),
        .led_lookup = .init(.{ .@"!" = parseErrorType, .@"<" = parseGenericType, .@"." = parseMemberType }),
    };
}

fn getBindingPower(self: *Self, token: lexer.TokenKind) BindingPower {
    return self.bp_lookup.get(token) orelse .default;
}

pub fn parseType(self: *Self, alloc: std.mem.Allocator, io: std.Io, precedence: BindingPower) Error!ast.Type {
    var token = self.parent_parser.currentToken();
    var left = if (self.nud_lookup.get(std.meta.activeTag(token))) |nud_handler|
        try nud_handler(self, alloc, io)
    else
        return error.HandlerDoesNotExist;

    while (@intFromEnum(precedence) < @intFromEnum(self.getBindingPower(self.parent_parser.currentToken()))) {
        token = self.parent_parser.advance(); // consume operator
        if (self.led_lookup.get(std.meta.activeTag(token))) |led_handler| {
            left = try led_handler(self, alloc, io, left, self.getBindingPower(std.meta.activeTag(token)));
        } else {
            return left;
        }
    }

    return left;
}

pub fn parseGenericType(
    self: *Self,
    alloc: std.mem.Allocator,
    io: std.Io,
    lhs: ast.Type,
    _: BindingPower,
) Error!ast.Type {
    var depth: usize = 1;
    for (self.parent_parser.input[self.parent_parser.pos..], self.parent_parser.pos..) |*token, idx| {
        switch (token.*) {
            .@"<" => depth += 1,
            .@">" => {
                depth -= 1;
                if (depth == 0) break;
            },
            .@">>" => {
                if (depth <= 2) {
                    token.* = .@">";
                    const pos_entry = self.parent_parser.source_map[idx];
                    var tokens: std.ArrayList(lexer.Token) = .fromOwnedSlice(self.parent_parser.input);
                    var source_map: std.ArrayList(utils.Position) = .fromOwnedSlice(self.parent_parser.source_map);

                    try tokens.insert(self.parent_parser.alloc, idx + 1, .@">");
                    try source_map.insert(self.parent_parser.alloc, idx + 1, pos_entry);

                    self.parent_parser.input = try tokens.toOwnedSlice(alloc);
                    self.parent_parser.source_map = try source_map.toOwnedSlice(alloc);
                    break;
                }
                depth -= 2;
            },
            else => {},
        }
    }

    // < is already consumed by parseType loop
    var args: std.ArrayList(ast.Expression) = .empty;

    if (self.parent_parser.currentToken() != .@">") {
        while (true) {
            const arg: ast.Expression = b: {
                const backup_pos = self.parent_parser.pos;
                const current_pos = self.parent_parser.pos;
                if (self.parseType(alloc, io, .default)) |t|
                    break :b .{ .type = .{ .pos = current_pos, .payload = t } }
                else |_| {
                    self.parent_parser.pos = backup_pos;
                    break :b try expressions.parse(self.parent_parser, .relational, .{});
                }
            };
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
            .pos = lhs.pos(), // Simplification: use start pos
            .lhs = ptr,
            .arguments = try args.toOwnedSlice(self.parent_parser.alloc),
        },
    };
}

pub fn parseSymbolType(self: *Self, _: std.mem.Allocator, _: std.Io) Error!ast.Type {
    const pos = self.parent_parser.pos;
    const ident = try self.parent_parser.expect(
        self.parent_parser.advance(),
        .ident,
        "type descriptor",
        "type name",
    );

    return .{ .symbol = .{ .inner = try self.parent_parser.alloc.dupe(u8, ident), .pos = pos } };
}

pub fn parseReferenceType(self: *Self, alloc: std.mem.Allocator, io: std.Io) Error!ast.Type {
    const pos = self.parent_parser.pos;
    _ = self.parent_parser.advance(); // consume '&'

    const is_mut = self.parent_parser.currentToken() == .mut;
    if (is_mut) _ = self.parent_parser.advance(); // consume `mut`

    const inner = try alloc.create(ast.Type);
    inner.* = try parseType(self, alloc, io, .default);

    return .{
        .reference = .{
            .pos = pos,
            .inner = inner,
            .is_mut = is_mut,
        },
    };
}

pub fn parseOptionalType(self: *Self, alloc: std.mem.Allocator, io: std.Io) Error!ast.Type {
    const pos = self.parent_parser.pos;
    _ = self.parent_parser.advance(); // consume '?'

    const inner = try alloc.create(ast.Type);
    inner.* = try parseType(self, alloc, io, .default);

    return .{
        .optional = .{
            .pos = pos,
            .inner = inner,
        },
    };
}

pub fn parseFunctionType(self: *Self, alloc: std.mem.Allocator, io: std.Io) Error!ast.Type {
    const pos = self.parent_parser.pos;
    _ = self.parent_parser.advance(); // consume "fn" keyword
    const generic_parameters: []const ast.Type = switch (self.parent_parser.currentToken()) {
        .@"(" => &.{},
        .@"<" => try self.parent_parser.parseTypeList(true),
        else => |other| return self.parent_parser.unexpectedToken("function type", "(' or '<", other),
    };
    const parameters = try self.parent_parser.parseTypeList(false);
    const return_type = try alloc.create(ast.Type);
    return_type.* = parseType(self, alloc, io, .default) catch |err| switch (err) {
        error.HandlerDoesNotExist, error.UnexpectedToken => return utils.printErr(
            io,
            error.MissingReturnType,
            "Parser error: missing return type in function type at {f}.\n",
            .{self.parent_parser.source_map[self.parent_parser.pos]},
        ),
        else => return err,
    };

    return .{
        .function = .{
            .pos = pos,
            .parameters = parameters,
            .generic_parameters = generic_parameters,
            .return_type = return_type,
        },
    };
}

pub fn parseErrorType(self: *Self, alloc: std.mem.Allocator, io: std.Io, lhs: ast.Type, _: BindingPower) Error!ast.Type {
    const pos = self.parent_parser.pos;

    const failure = try alloc.create(ast.Type);
    failure.* = lhs;

    const success = try alloc.create(ast.Type);
    success.* = try parseType(self, alloc, io, .default);

    return .{
        .error_union = .{
            .pos = pos,
            .success = success,
            .failure = failure,
        },
    };
}

pub fn parseArrayType(self: *Self, alloc: std.mem.Allocator, io: std.Io) Error!ast.Type {
    const pos = self.parent_parser.pos;
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
    inner.* = try self.parseType(alloc, io, .default);

    return if (size) |s| .{
        .array = .{
            .pos = pos,
            .inner = inner,
            .size = s,
        },
    } else .{
        .slice = .{
            .pos = pos,
            .inner = inner,
            .is_mut = is_mut.?,
        },
    };
}

pub fn parseGroupType(self: *Self, alloc: std.mem.Allocator, io: std.Io) Error!ast.Type {
    try self.parent_parser.expect(self.parent_parser.advance(), .@"(", "group expression", "(");
    const @"type" = try parseType(self, alloc, io, .default);
    try self.parent_parser.expect(self.parent_parser.advance(), .@")", "group expression", ")");

    return @"type";
}

pub fn parseVariadic(self: *Self, _: std.mem.Allocator, _: std.Io) Error!ast.Type {
    const pos = self.parent_parser.pos;
    _ = self.parent_parser.advance();
    return .{ .variadic = .{ .pos = pos } };
}

pub fn parseMemberType(
    self: *Self,
    alloc: std.mem.Allocator,
    _: std.Io,
    lhs: ast.Type,
    _: BindingPower,
) Error!ast.Type {
    const pos = self.parent_parser.pos;
    const member_name = try self.parent_parser.expect(
        self.parent_parser.advance(),
        .ident,
        "member type",
        "member name",
    );

    const lhs_ptr = try alloc.create(ast.Type);
    lhs_ptr.* = lhs;

    return .{
        .member = .{
            .pos = pos,
            .parent = lhs_ptr,
            .member_name = try self.parent_parser.alloc.dupe(u8, member_name),
        },
    };
}

inline fn getHandler(
    self: *const Self,
    comptime handler_type: enum { nud, led, bp },
    token: lexer.TokenKind,
) Error!switch (handler_type) {
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
    );
}

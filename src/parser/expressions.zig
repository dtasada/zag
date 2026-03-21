const std = @import("std");
const utils = @import("utils");
const ast = @import("ast");
const statements = @import("statements.zig");

const TypeParser = @import("TypeParser.zig");

const parser = @import("parser.zig");
const Parser = parser.Parser;
const Error = parser.Error;
const BindingPower = parser.BindingPower;

pub fn primary(self: *Parser) !ast.Expression {
    const pos = self.pos;

    var expr: ast.Expression = switch (self.advance()) {
        inline .int, .float, .char => |n, t| @unionInit(
            ast.Expression,
            @tagName(t),
            .{ .pos = pos, .payload = n },
        ),
        inline .ident, .string => |s, t| @unionInit(
            ast.Expression,
            @tagName(t),
            .{ .pos = pos, .payload = try self.alloc.dupe(u8, s) },
        ),
        else => |other| return self.unexpectedToken("primary expression", "(int | float | ident | string | char)", other),
    };
    errdefer expr.deinit(self.alloc);

    if (expr == .ident and self.currentToken() == .@"<") {
        if (isGenericLookahead(self)) {
            const old_expr = try self.alloc.create(ast.Expression);
            errdefer self.alloc.destroy(old_expr);

            old_expr.* = expr;
            expr = try generic(self, old_expr, .default);
        }
    }

    return expr;
}

pub fn binary(self: *Parser, lhs: *const ast.Expression, bp: BindingPower) Error!ast.Expression {
    const pos = self.pos;
    const op: ast.BinaryOperator = .fromLexerToken(self.advance());

    const new_rhs = try self.alloc.create(ast.Expression);
    new_rhs.* = try parse(self, bp, .{});
    errdefer new_rhs.deinitPtr(self.alloc);

    switch (op) {
        .@"==", .@"!=", .@"<", .@">", .@"<=", .@">=" => {
            if (lhs.* == .comparison) {
                var comparisons: std.ArrayList(ast.Expression.Comparison.Item) =
                    .fromOwnedSlice(try utils.cloneSlice(
                        ast.Expression.Comparison.Item,
                        lhs.comparison.comparisons,
                        self.alloc,
                    ));
                errdefer comparisons.deinit(self.alloc);
                try comparisons.append(self.alloc, .{ .op = op, .right = new_rhs });
                return .{
                    .comparison = .{
                        .pos = lhs.getPosition(),
                        .left = lhs.comparison.left,
                        .comparisons = try comparisons.toOwnedSlice(self.alloc),
                    },
                };
            }

            if (lhs.* == .binary) switch (lhs.binary.op) {
                .@"==", .@"!=", .@"<", .@">", .@"<=", .@">=" => return .{
                    .comparison = .{
                        .pos = lhs.getPosition(),
                        .left = lhs.binary.lhs,
                        .comparisons = &.{
                            .{ .op = lhs.binary.op, .right = lhs.binary.rhs },
                            .{ .op = op, .right = new_rhs },
                        },
                    },
                },
                else => {},
            };

            return .{
                .binary = .{
                    .pos = pos,
                    .lhs = lhs,
                    .op = op,
                    .rhs = new_rhs,
                },
            };
        },
        else => return .{
            .binary = .{
                .pos = pos,
                .lhs = lhs,
                .op = op,
                .rhs = new_rhs,
            },
        },
    }
}

pub fn parse(self: *Parser, bp: BindingPower, opts: struct { silent_error: bool = false }) Error!ast.Expression {
    // first parse the NUD
    const nud_fn = try self.getHandler(.nud, self.currentToken(), .{ .silent_error = opts.silent_error });
    var lhs = try nud_fn(self);
    errdefer lhs.deinit(self.alloc);

    // while we have a led and (current bp < bp of current token)
    // continue parsing lhs
    while (self.bp_lookup.get(self.currentToken())) |current_bp| {
        if (@intFromEnum(current_bp) <= @intFromEnum(bp)) break;

        // This should be an assertion, since we found a bp
        const led_fn = try self.getHandler(.led, self.currentToken(), .{ .silent_error = opts.silent_error });

        const old_lhs = try self.alloc.create(ast.Expression);
        errdefer self.alloc.destroy(old_lhs);
        old_lhs.* = lhs;

        lhs = try led_fn(self, old_lhs, try self.getHandler(.bp, self.currentToken(), .{ .silent_error = opts.silent_error }));
    }

    return lhs;
}

pub fn assignment(self: *Parser, lhs: *const ast.Expression, bp: BindingPower) Error!ast.Expression {
    const pos = self.pos;
    const op = self.advance();

    const rhs = try self.alloc.create(ast.Expression);
    errdefer self.alloc.destroy(rhs);
    rhs.* = try parse(self, bp, .{});

    return .{
        .assignment = .{
            .pos = pos,
            .assignee = lhs,
            .op = .fromLexerToken(op),
            .value = rhs,
        },
    };
}

pub fn member(self: *Parser, lhs: *const ast.Expression, _: BindingPower) Error!ast.Expression {
    const pos = self.pos;
    _ = self.advance(); // consume dot
    switch (self.currentToken()) {
        .ident => |ident| {
            _ = self.advance();
            return .{ .member = .{ .pos = pos, .parent = lhs, .member_name = try self.alloc.dupe(u8, ident) } };
        },
        .@"*" => {
            _ = self.advance();
            return .{ .dereference = .{ .pos = pos, .parent = lhs } };
        },
        else => |other| return self.unexpectedToken("member expression", "member name or dereference", other),
    }
}

pub fn prefix(self: *Parser) Error!ast.Expression {
    const pos = self.pos;
    const op = self.advance();

    const rhs = try self.alloc.create(ast.Expression);
    errdefer self.alloc.destroy(rhs);
    rhs.* = try parse(self, .unary, .{});

    return .{
        .prefix = .{
            .pos = pos,
            .op = .fromLexerToken(op),
            .rhs = rhs,
        },
    };
}

pub fn group(self: *Parser) Error!ast.Expression {
    try self.expect(self.advance(), .@"(", "group expression", "(");
    const expr = try parse(self, .default, .{});
    errdefer expr.deinit(self.alloc);
    try self.expect(self.advance(), .@")", "group expression", ")");

    return expr;
}

pub fn structInstantiation(self: *Parser, lhs: *const ast.Expression, _: BindingPower) Error!ast.Expression {
    try self.expect(self.advance(), .@"{", "struct instantiation", "{");

    var @"struct": ast.Expression.StructInstantiation = .{
        .pos = lhs.getPosition(),
        .type_expr = lhs,
        .members = b: {
            const members = try self.alloc.create(std.StringHashMap(ast.Expression));
            members.* = .init(self.alloc);
            break :b members;
        },
    };
    errdefer {
        var members_it = @"struct".members.iterator();
        while (members_it.next()) |entry| {
            self.alloc.free(entry.key_ptr.*);
            entry.value_ptr.deinit(self.alloc);
        }
        @"struct".members.deinit();
        self.alloc.destroy(@"struct".members);
    }

    while (self.currentToken() != .eof and self.currentToken() != .@"}") {
        const member_name = try self.expect(self.advance(), .ident, "struct instantiation", "struct member name");
        try self.expect(self.advance(), .@":", "struct instantiation", ":");
        const member_value = try parse(self, .default, .{});
        errdefer member_value.deinit(self.alloc);

        try @"struct".members.put(try self.alloc.dupe(u8, member_name), member_value);

        if (self.currentToken() != .@"}") try self.expect(self.advance(), .@",", "struct instantiation", ",");
    }

    try self.expect(self.advance(), .@"}", "struct instantiation", "}");

    return .{ .struct_instantiation = @"struct" };
}

pub fn arrayInstantiation(self: *Parser) Error!ast.Expression {
    const pos = self.pos;
    const backup_pos = self.pos;

    try self.expect(self.advance(), .@"[", "array instantiation", "[");

    if (self.currentToken() == .@"]") {
        self.pos = backup_pos;
        return .{
            .type = .{
                .pos = pos,
                .payload = try self.type_parser.parseType(self.alloc, .primary),
            },
        };
    }

    const length = try self.alloc.create(ast.Expression);
    errdefer self.alloc.destroy(length);
    length.* = try parse(self, .default, .{});
    errdefer length.deinit(self.alloc);
    try self.expect(self.advance(), .@"]", "array instantiation", "]");

    return .{
        .array_instantiation = .{
            .pos = pos,
            .type = try self.type_parser.parseType(self.alloc, .default),
            .length = length,
            .contents = b: {
                var contents: std.ArrayList(ast.Expression) = .empty;
                errdefer utils.deinitArrayList(ast.Expression, &contents, self.alloc);

                try self.expect(self.advance(), .@"{", "array instantiation", "{");
                while (self.currentToken() != .eof and self.currentToken() != .@"}") {
                    try contents.append(self.alloc, try parse(self, .logical, .{}));

                    if (self.currentToken() != .@"}")
                        try self.expect(self.advance(), .@",", "array literal", ",");
                }
                try self.expect(self.advance(), .@"}", "array instantiation", "}");

                break :b try contents.toOwnedSlice(self.alloc);
            },
        },
    };
}

pub fn call(self: *Parser, lhs: *const ast.Expression, _: BindingPower) Error!ast.Expression {
    return .{
        .call = .{
            .pos = lhs.getPosition(),
            .callee = lhs,
            .args = try self.parseArguments(),
        },
    };
}

pub fn ambiguousLessThan(self: *Parser, lhs: *const ast.Expression, bp: BindingPower) Error!ast.Expression {
    return if (isGenericLookahead(self)) generic(self, lhs, bp) else binary(self, lhs, .relational);
}

fn isGenericLookahead(self: *Parser) bool {
    var depth: usize = 0;

    for (self.input[self.pos..]) |token| switch (token) {
        .@"<" => depth += 1,
        .@">" => {
            depth -= 1;
            if (depth == 0) return true;
        },
        .@">>" => {
            if (depth <= 2) return true;
            depth -= 2;
        },
        .@"(", .@")", .@"{", .@"}", .@";" => return false,
        else => {},
    };

    return false;
}

pub fn generic(self: *Parser, lhs: *const ast.Expression, _: BindingPower) Error!ast.Expression {
    return .{
        .generic = .{
            .pos = lhs.getPosition(),
            .lhs = lhs,
            .arguments = try self.parseGenericArguments(),
        },
    };
}

pub fn @"if"(self: *Parser) Error!ast.Expression {
    const pos = self.pos;
    try self.expect(self.advance(), .@"if", "if expression", "if");
    try self.expect(self.advance(), .@"(", "if expression", "(");

    const condition = try self.alloc.create(ast.Expression);
    errdefer self.alloc.destroy(condition);
    condition.* = try parse(self, .default, .{});
    errdefer condition.deinit(self.alloc);

    try self.expect(self.advance(), .@")", "if expression", ")");

    const capture = try self.parseCapture();
    errdefer if (capture) |c| c.deinit(self.alloc);

    const body = try self.alloc.create(ast.Expression);
    errdefer self.alloc.destroy(body);
    body.* = if (self.currentToken() == .@"{")
        try block(self)
    else
        try parse(self, .default, .{});
    errdefer body.deinit(self.alloc);

    var @"else": ?*ast.Expression = null;
    errdefer if (@"else") |e| e.deinitPtr(self.alloc);
    if (self.currentToken() == .@"{") {
        // block
    } else if (self.currentToken() == .@"else") {
        _ = self.advance(); // consume `else`

        @"else" = try self.alloc.create(ast.Expression);
        errdefer self.alloc.destroy(@"else".?);

        @"else".?.* = if (self.currentToken() == .@"{")
            try block(self)
        else
            try parse(self, .default, .{});
    }

    return .{
        .@"if" = .{
            .pos = pos,
            .condition = condition,
            .capture = capture,
            .body = body,
            .@"else" = @"else",
        },
    };
}

pub fn match(self: *Parser) Error!ast.Expression {
    const position = self.pos;
    _ = self.advance(); // consume `match` keyword

    try self.expect(self.advance(), .@"(", "match statement", "(");
    const condition = try self.alloc.create(ast.Expression);
    errdefer self.alloc.destroy(condition);
    condition.* = try parse(self, .default, .{});
    errdefer condition.deinit(self.alloc);
    try self.expect(self.advance(), .@")", "match statement", ")");

    try self.expect(self.advance(), .@"{", "match statement", "{");

    var cases: std.ArrayList(ast.Expression.Match.Case) = .empty;
    errdefer utils.deinitArrayList(ast.Expression.Match.Case, &cases, self.alloc);
    while (true) {
        const pos = self.pos;

        // var cond: std.ArrayList(ast.Expression) = .empty;
        const cond: ast.Expression.Match.Case.Condition = if (self.currentToken() == .@"else") b: {
            _ = self.advance();
            break :b .@"else";
        } else b: {
            var conds: std.ArrayList(ast.Expression) = .empty;
            errdefer utils.deinitArrayList(ast.Expression, &conds, self.alloc);
            try conds.append(self.alloc, try parse(self, .default, .{}));

            while (self.currentToken() == .@",") {
                _ = self.advance();
                try conds.append(self.alloc, try parse(self, .default, .{}));
            }

            break :b .{ .opts = try conds.toOwnedSlice(self.alloc) };
        };

        try self.expect(self.advance(), .@"->", "match statement case", "->");

        const result: ast.Statement = b: {
            if (self.statement_lookup.get(self.currentToken())) |statement_fn| {
                self.expect_semicolon = false;
                defer self.expect_semicolon = true;

                break :b try statement_fn(self);
            }

            break :b .{ .expression = try parse(self, .default, .{}) };
        };
        errdefer result.deinit(self.alloc);

        try cases.append(self.alloc, .{ .condition = cond, .pos = pos, .result = result });

        self.expectSilent(self.currentToken(), .@",") catch break;
        _ = self.advance();
        if (self.currentToken() == .@"}") break;
    }

    try self.expect(self.advance(), .@"}", "match statement", "}");

    return .{
        .match = .{
            .pos = position,
            .condition = condition,
            .cases = try cases.toOwnedSlice(self.alloc),
        },
    };
}

pub fn block(self: *Parser) Error!ast.Expression {
    return .{
        .block = .{
            .pos = self.pos,
            .payload = try self.parseBlock(),
        },
    };
}

pub fn range(self: *Parser, lhs: *const ast.Expression, _: BindingPower) Error!ast.Expression {
    const token = self.advance(); // move past '..|..='

    const end = if (parse(self, .default, .{ .silent_error = true })) |end| b: {
        const ptr = try self.alloc.create(ast.Expression);
        ptr.* = end;
        break :b ptr;
    } else |err| switch (err) {
        error.HandlerDoesNotExist => null,
        else => return err,
    };

    return .{
        .range = .{
            .pos = lhs.getPosition(),
            .start = lhs,
            .end = end,
            .inclusive = token != .@"..",
        },
    };
}

pub fn index(self: *Parser, lhs: *const ast.Expression, _: BindingPower) Error!ast.Expression {
    _ = self.advance(); // move past '['

    if (self.currentToken() == .@".." or self.currentToken() == .@"..=") {
        const inclusive = self.advance() == .@"..=";

        const end = try self.alloc.create(ast.Expression);
        errdefer self.alloc.destroy(end);
        end.* = parse(self, .default, .{}) catch |err| switch (err) {
            error.HandlerDoesNotExist => return utils.printErr(
                error.SyntaxError,
                "Parser error: slice expression with no start index must contain an end index ({f}).\n",
                .{self.source_map[self.pos]},
            ),
            else => return err,
        };
        errdefer end.deinit(self.alloc);

        try self.expect(self.advance(), .@"]", "index expression", "]");

        return .{
            .slice = .{
                .pos = lhs.getPosition(),
                .lhs = lhs,
                .start = null,
                .end = end,
                .inclusive = inclusive,
            },
        };
    }

    const i = try self.alloc.create(ast.Expression);
    errdefer self.alloc.destroy(i);
    i.* = try parse(self, .default, .{});
    errdefer i.deinit(self.alloc);
    defer if (i.* == .range) i.deinitPtr(self.alloc);

    const expr: ast.Expression = if (i.* == .range) .{
        .slice = .{
            .pos = lhs.getPosition(),
            .lhs = lhs,
            .start = try i.range.start.clonePtr(self.alloc),
            .end = if (i.range.end) |e| try e.clonePtr(self.alloc) else null,
            .inclusive = i.range.inclusive,
        },
    } else .{
        .index = .{
            .pos = lhs.getPosition(),
            .lhs = lhs,
            .index = i,
        },
    };

    try self.expect(self.advance(), .@"]", "index expression", "]");

    return expr;
}

pub fn reference(self: *Parser) Error!ast.Expression {
    const pos = self.pos;
    _ = self.advance(); // consume `&`

    const is_mut = self.currentToken() == .mut;
    if (is_mut) _ = self.advance(); // consume `mut`

    const inner = try self.alloc.create(ast.Expression);
    errdefer self.alloc.destroy(inner);
    inner.* = try parse(self, .default, .{});

    return .{
        .reference = .{
            .pos = pos,
            .inner = inner,
            .is_mut = is_mut,
        },
    };
}

pub fn @"try"(self: *Parser) Error!ast.Expression {
    const pos = self.pos;
    _ = self.advance();

    const expr = try self.alloc.create(ast.Expression);
    errdefer self.alloc.destroy(expr);
    expr.* = try parse(self, .default, .{});

    return .{ .@"try" = .{ .payload = expr, .pos = pos } };
}

pub fn functionType(self: *Parser) !ast.Expression {
    const pos = self.pos;
    const type_node = try self.type_parser.parseType(self.alloc, .primary);
    return .{ .type = .{ .pos = pos, .payload = type_node } };
}

pub fn @"catch"(self: *Parser, lhs: *const ast.Expression, binding_power: BindingPower) Error!ast.Expression {
    _ = self.advance(); // move past 'catch'

    const rhs = try self.alloc.create(ast.Expression);
    errdefer self.alloc.destroy(rhs);
    rhs.* = try parse(self, binding_power, .{});

    return .{
        .@"catch" = .{
            .pos = lhs.getPosition(),
            .lhs = lhs,
            .rhs = rhs,
        },
    };
}

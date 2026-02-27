const std = @import("std");
const utils = @import("utils");
const ast = @import("ast.zig");
const statements = @import("statements.zig");

const Lexer = @import("Lexer");
const TypeParser = @import("TypeParser.zig");

const Self = @import("Parser.zig");
const ParserError = Self.ParserError;
const BindingPower = Self.BindingPower;

pub fn primary(self: *Self) !ast.Expression {
    const pos = self.currentPosition();

    var expr: ast.Expression = switch (self.advance()) {
        .int => |int| .{ .uint = .{ .pos = pos, .uint = int } },
        .float => |float| .{ .float = .{ .pos = pos, .float = float } },
        .ident => |ident| .{ .ident = .{ .pos = pos, .ident = ident } },
        .string => |string| .{ .string = .{ .pos = pos, .string = string } },
        .char => |char| .{ .char = .{ .pos = pos, .char = char } },
        else => |other| return self.unexpectedToken("primary expression", "(int | float | ident | string | char)", other),
    };

    if (expr == .ident and self.currentToken() == .@"<") {
        if (isGenericLookahead(self)) {
            const old_expr = try self.alloc.create(ast.Expression);
            old_expr.* = expr;
            expr = try generic(self, old_expr, .default);
        }
    }

    return expr;
}

pub fn binary(self: *Self, lhs: *const ast.Expression, bp: BindingPower) ParserError!ast.Expression {
    const pos = self.currentPosition();
    const op: ast.BinaryOperator = .fromLexerToken(self.advance());
    const rhs = try parse(self, bp, .{});

    const new_rhs = try self.alloc.create(ast.Expression);
    new_rhs.* = rhs;

    switch (op) {
        .@"==", .@"!=", .@"<", .@">", .@"<=", .@">=" => {
            if (lhs.* == .comparison) {
                var comparisons = try lhs.comparison.comparisons.clone(self.alloc);
                try comparisons.append(self.alloc, .{ .op = op, .right = new_rhs });
                return .{
                    .comparison = .{
                        .pos = lhs.getPosition(),
                        .left = lhs.comparison.left,
                        .comparisons = comparisons,
                    },
                };
            }

            if (lhs.* == .binary) {
                switch (lhs.binary.op) {
                    .@"==", .@"!=", .@"<", .@">", .@"<=", .@">=" => {
                        var comparisons: std.ArrayList(ast.Expression.Comparison.Item) = .empty;
                        try comparisons.append(self.alloc, .{ .op = lhs.binary.op, .right = lhs.binary.rhs });
                        try comparisons.append(self.alloc, .{ .op = op, .right = new_rhs });
                        return .{
                            .comparison = .{
                                .pos = lhs.getPosition(),
                                .left = lhs.binary.lhs,
                                .comparisons = comparisons,
                            },
                        };
                    },
                    else => {},
                }
            }

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

pub fn parse(self: *Self, bp: BindingPower, opts: struct { silent_error: bool = false }) ParserError!ast.Expression {
    // first parse the NUD
    const nud_fn = try self.getHandler(.nud, self.currentToken(), .{ .silent_error = opts.silent_error });
    var lhs = try nud_fn(self);

    // while we have a led and (current bp < bp of current token)
    // continue parsing lhs
    while (self.bp_lookup.get(self.currentToken())) |current_bp| {
        if (@intFromEnum(current_bp) <= @intFromEnum(bp)) break;

        // This should be an assertion, since we found a bp
        const led_fn = try self.getHandler(.led, self.currentToken(), .{ .silent_error = opts.silent_error });

        const old_lhs = try self.alloc.create(ast.Expression);
        old_lhs.* = lhs;

        lhs = try led_fn(self, old_lhs, try self.getHandler(.bp, self.currentToken(), .{ .silent_error = opts.silent_error }));
    }

    return lhs;
}

pub fn assignment(self: *Self, lhs: *const ast.Expression, bp: BindingPower) ParserError!ast.Expression {
    const pos = self.currentPosition();
    const op = self.advance();

    const rhs = try self.alloc.create(ast.Expression);
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

pub fn member(self: *Self, lhs: *const ast.Expression, _: BindingPower) ParserError!ast.Expression {
    const pos = self.currentPosition();
    _ = self.advance(); // consume dot
    const member_name = try self.expect(self.advance(), .ident, "member expression", "member name");

    return .{
        .member = .{
            .pos = pos,
            .parent = lhs,
            .member_name = member_name,
        },
    };
}

pub fn prefix(self: *Self) ParserError!ast.Expression {
    const pos = self.currentPosition();
    const op = self.advance();

    const rhs = try self.alloc.create(ast.Expression);
    rhs.* = try parse(self, .unary, .{});

    return .{
        .prefix = .{
            .pos = pos,
            .op = .fromLexerToken(op),
            .rhs = rhs,
        },
    };
}

pub fn group(self: *Self) ParserError!ast.Expression {
    try self.expect(self.advance(), .@"(", "group expression", "(");
    const expr = try parse(self, .default, .{});
    try self.expect(self.advance(), .@")", "group expression", ")");

    return expr;
}

pub fn structInstantiation(self: *Self, lhs: *const ast.Expression, _: BindingPower) ParserError!ast.Expression {
    try self.expect(self.advance(), .@"{", "struct instantiation", "{");

    var @"struct": ast.Expression.StructInstantiation = .{
        .pos = lhs.getPosition(),
        .type_expr = lhs,
        .members = .init(self.alloc),
    };

    while (self.currentToken() != .eof and self.currentToken() != .@"}") {
        const member_name = try self.expect(self.advance(), .ident, "struct instantiation", "struct member name");
        try self.expect(self.advance(), .@":", "struct instantiation", ":");
        const member_value = try parse(self, .default, .{});

        try @"struct".members.put(member_name, member_value);

        if (self.currentToken() != .@"}") {
            try self.expect(self.advance(), .@",", "struct instantiation", ",");
        }
    }

    try self.expect(self.advance(), .@"}", "struct instantiation", "}");

    return .{ .struct_instantiation = @"struct" };
}

pub fn arrayInstantiation(self: *Self) ParserError!ast.Expression {
    const pos = self.currentPosition();

    try self.expect(self.advance(), .@"[", "array instantiation", "[");
    const length = try self.alloc.create(ast.Expression);
    length.* = try parse(self, .default, .{});
    try self.expect(self.advance(), .@"]", "array instantiation", "]");

    var array: ast.Expression.ArrayInstantiation = .{
        .pos = pos,
        .type = try self.type_parser.parseType(self.alloc, .default),
        .length = length,
    };

    try self.expect(self.advance(), .@"{", "array instantiation", "{");
    while (self.currentToken() != .eof and self.currentToken() != .@"}") {
        try array.contents.append(self.alloc, try parse(self, .logical, .{}));

        if (self.currentToken() != .@"}")
            try self.expect(self.advance(), .@",", "array literal", ",");
    }
    try self.expect(self.advance(), .@"}", "array instantiation", "}");

    return .{ .array_instantiation = array };
}

pub fn call(self: *Self, lhs: *const ast.Expression, _: BindingPower) ParserError!ast.Expression {
    return .{
        .call = .{
            .pos = lhs.getPosition(),
            .callee = lhs,
            .args = try self.parseArguments(),
        },
    };
}

pub fn ambiguousLessThan(self: *Self, lhs: *const ast.Expression, bp: BindingPower) ParserError!ast.Expression {
    return if (isGenericLookahead(self)) generic(self, lhs, bp) else binary(self, lhs, .relational);
}

fn isGenericLookahead(self: *Self) bool {
    var depth: usize = 0;

    for (self.lexer.tokens.items[self.pos..]) |token| switch (token) {
        .@"<" => depth += 1,
        .@">" => {
            depth -= 1;
            if (depth == 0) return true;
        },
        .@"(", .@")", .@"{", .@"}", .@";" => return false,
        else => {},
    };

    return false;
}

pub fn generic(self: *Self, lhs: *const ast.Expression, _: BindingPower) ParserError!ast.Expression {
    const args = try self.parseGenericArguments();
    return .{
        .generic = .{
            .pos = lhs.getPosition(),
            .lhs = lhs,
            .arguments = args,
        },
    };
}

pub fn @"if"(self: *Self) ParserError!ast.Expression {
    const pos = self.currentPosition();
    try self.expect(self.advance(), .@"if", "if expression", "if");

    try self.expect(self.advance(), .@"(", "if expression", "(");

    const condition = try self.alloc.create(ast.Expression);
    condition.* = try parse(self, .default, .{});

    try self.expect(self.advance(), .@")", "if expression", ")");

    const capture = try self.parseCapture();

    const body = try self.alloc.create(ast.Expression);
    body.* = if (self.currentToken() == .@"{")
        try block(self)
    else
        try parse(self, .default, .{});

    var @"else": ?*ast.Expression = null;
    if (self.currentToken() == .@"{") {
        // block
    } else if (self.currentToken() == .@"else") {
        _ = self.advance(); // consume `else`

        @"else" = try self.alloc.create(ast.Expression);
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

pub fn match(self: *Self) ParserError!ast.Expression {
    const position = self.currentPosition();
    _ = self.advance(); // consume `match` keyword

    try self.expect(self.advance(), .@"(", "match statement", "(");
    const condition = try self.alloc.create(ast.Expression);
    condition.* = try parse(self, .default, .{});
    try self.expect(self.advance(), .@")", "match statement", ")");

    try self.expect(self.advance(), .@"{", "match statement", "{");

    var cases: std.ArrayList(ast.Expression.Match.Case) = .empty;
    while (true) {
        const pos = self.currentPosition();

        // var cond: std.ArrayList(ast.Expression) = .empty;
        const cond: ast.Expression.Match.Case.Condition = if (self.currentToken() == .@"else") b: {
            _ = self.advance();
            break :b .@"else";
        } else b: {
            var conds: std.ArrayList(ast.Expression) = .empty;
            try conds.append(self.alloc, try parse(self, .default, .{}));

            while (self.currentToken() == .@",") {
                _ = self.advance();
                try conds.append(self.alloc, try parse(self, .default, .{}));
            }

            break :b .{ .opts = conds };
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
            .cases = cases,
        },
    };
}

pub fn block(self: *Self) ParserError!ast.Expression {
    return .{ .block = .{ .pos = self.currentPosition(), .block = try self.parseBlock() } };
}

pub fn range(self: *Self, lhs: *const ast.Expression, _: BindingPower) ParserError!ast.Expression {
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

pub fn index(self: *Self, lhs: *const ast.Expression, _: BindingPower) ParserError!ast.Expression {
    _ = self.advance(); // move past '['

    if (self.currentToken() == .@".." or self.currentToken() == .@"..=") {
        const inclusive = self.advance() == .@"..=";

        const end = try self.alloc.create(ast.Expression);
        end.* = parse(self, .default, .{}) catch |err| switch (err) {
            error.HandlerDoesNotExist => return utils.printErr(
                error.SyntaxError,
                "Parser error: slice expression with no start index must contain an end index ({f}).\n",
                .{self.currentPosition()},
                .red,
            ),
            else => return err,
        };

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
    i.* = try parse(self, .default, .{});

    const expr: ast.Expression = if (i.* == .range) .{
        .slice = .{
            .pos = lhs.getPosition(),
            .lhs = lhs,
            .start = i.range.start,
            .end = i.range.end,
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

pub fn reference(self: *Self) ParserError!ast.Expression {
    const pos = self.currentPosition();
    _ = self.advance(); // consume `&`

    const is_mut = self.currentToken() == .mut;
    if (is_mut) _ = self.advance(); // consume `mut`

    const inner = try self.alloc.create(ast.Expression);
    inner.* = try parse(self, .default, .{});

    return .{
        .reference = .{
            .pos = pos,
            .inner = inner,
            .is_mut = is_mut,
        },
    };
}

pub fn @"try"(self: *Self) ParserError!ast.Expression {
    const pos = self.currentPosition();
    _ = self.advance();

    const expr = try self.alloc.create(ast.Expression);
    expr.* = try parse(self, .default, .{});

    return .{ .@"try" = .{ .@"try" = expr, .pos = pos } };
}

pub fn functionType(self: *Self) !ast.Expression {
    const type_node = try self.type_parser.parseType(self.alloc, .primary);
    return .{ .type = type_node };
}

pub fn @"catch"(self: *Self, lhs: *const ast.Expression, binding_power: BindingPower) ParserError!ast.Expression {
    _ = self.advance(); // move past 'catch'

    const rhs = try self.alloc.create(ast.Expression);
    rhs.* = try parse(self, binding_power, .{});

    return .{
        .@"catch" = .{
            .pos = lhs.getPosition(),
            .lhs = lhs,
            .rhs = rhs,
        },
    };
}

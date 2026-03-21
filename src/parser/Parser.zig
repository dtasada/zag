const std = @import("std");
const utils = @import("utils");
const ast = @import("ast");

const statements = @import("statements.zig");
const top_level_statements = @import("top_level_statements.zig");
const expressions = @import("expressions.zig");

const lexer = @import("lexer");
const TypeParser = @import("TypeParser.zig");

/// Function signature for a statement handler.
const StatementHandler = *const fn (*Parser) Error!ast.Statement;

/// Function signature for a statement handler.
pub const TopLevelStatementHandler = *const fn (*Parser) Error!ast.TopLevelStatement;

/// Function signature for a nud handler.
const NudHandler = *const fn (*Parser) Error!ast.Expression;

/// Function signature for a led handler.
const LedHandler = *const fn (*Parser, *const ast.Expression, BindingPower) Error!ast.Expression;

/// Binding power. Order of this enum is functional, so don't change it.
pub const BindingPower = enum {
    default,
    @",",
    assignment,
    logical,
    relational,
    additive,
    multiplicative,
    unary,
    call,
    power,
    member,
    primary,
};

pub const Error = error{
    UnexpectedToken,
    UnexpectedExpression,
    NoSpaceLeft,
    HandlerDoesNotExist,
    OutOfMemory,
    ExpressionNotInMap,
    StatementNotInMap,
    MissingReturnType,
    SyntaxError,
    MustBeTopLevelStatement,
};

pub const Parser = struct {
    /// Parser state: the current reading position in the lexer token list.
    pos: usize,

    input: []lexer.Token,
    source_map: []utils.Position,

    alloc: std.mem.Allocator,

    /// Parser output: the root AST object.
    output: ast.RootNode = &.{},

    /// pratt parsing helpers
    type_parser: TypeParser,

    /// Maps a `Token` to its corresponding binding power.
    bp_lookup: std.EnumMap(lexer.TokenKind, BindingPower),

    /// Maps a `Token` to a corresponding nud handler.
    nud_lookup: std.EnumMap(lexer.TokenKind, NudHandler),

    /// Maps a `Token` to a corresponding led handler.
    led_lookup: std.EnumMap(lexer.TokenKind, LedHandler),

    /// Maps a `Token` to a corresponding statement handler.
    statement_lookup: std.EnumMap(lexer.TokenKind, StatementHandler),

    /// Maps a `Token` to a corresponding statement handler.
    top_level_statement_lookup: std.EnumMap(lexer.TokenKind, TopLevelStatementHandler),

    expect_semicolon: bool,

    // errors: std.ArrayList(Error) = .empty,

    /// Consumes current token and then increases position.
    pub inline fn advance(self: *Parser) lexer.Token {
        self.pos += 1;
        return self.previousToken();
    }

    /// Returns token at the current position.
    pub inline fn currentToken(self: *const Parser) lexer.Token {
        return self.input[self.pos];
    }

    /// Returns token at the current position.
    pub inline fn previousToken(self: *const Parser) lexer.Token {
        return self.input[self.pos - 1];
    }

    /// Prints error message and always returns an error.
    /// `environment` and `expected_token` are strings for the error message.
    /// example: "function definition" and "function name" respectively yields:
    /// "Unexpected token in function definition '<actual>' at <line>:<col>. Expected 'function_name'",
    pub fn unexpectedToken(
        self: *const Parser,
        environment: []const u8,
        expected_token: []const u8,
        actual: lexer.Token,
    ) error{UnexpectedToken} {
        return utils.printErr(
            error.UnexpectedToken,
            "Unexpected token '{f}' in {s} at {f}. Expected '{s}'\n",
            .{ actual, environment, self.source_map[self.pos - 1], expected_token },
        );
    }

    /// Returns active tag if active type of `actual` is the same as `expected`. Errors otherwise.
    /// `environment` and `expected_token` are strings for the error message.
    /// example: "function definition" and "function name" respectively yields
    /// "Unexpected token in function definition '<bad_token>' at <source_file>:<line>:<col>. Expected 'function_name'",
    pub fn expect(
        self: *const Parser,
        actual: lexer.Token,
        comptime expected: lexer.TokenKind,
        comptime context: []const u8,
        comptime expected_token: []const u8,
    ) !@FieldType(lexer.Token, @tagName(expected)) {
        return if (std.meta.activeTag(actual) == expected)
            @field(actual, @tagName(expected))
        else if (expected == .@";") utils.printErr(
            error.SyntaxError,
            "Parser error: expected semicolon after {s}, received '{f}' ({f}).\n",
            .{ context, actual, self.source_map[self.pos] },
        ) else self.unexpectedToken(context, expected_token, actual);
    }

    /// Identical to `expect` but doesn't print error message.
    pub fn expectSilent(
        _: *const Parser,
        actual: lexer.Token,
        comptime expected: lexer.TokenKind,
    ) !@FieldType(lexer.Token, @tagName(expected)) {
        return if (std.meta.activeTag(actual) == expected)
            @field(actual, @tagName(expected))
        else
            error.UnexpectedToken;
    }

    /// Returns the appropriate handler and errors if a handler isn't found.
    pub inline fn getHandler(
        self: *const Parser,
        comptime handler_type: enum { statement, nud, led, bp },
        token: lexer.TokenKind,
        opts: struct { silent_error: bool = false },
    ) Error!switch (handler_type) {
        .statement => StatementHandler,
        .nud => NudHandler,
        .led => LedHandler,
        .bp => BindingPower,
    } {
        return if (switch (handler_type) {
            .statement => self.statement_lookup,
            .nud => self.nud_lookup,
            .led => self.led_lookup,
            .bp => self.bp_lookup,
        }.get(token)) |handler| handler else return if (opts.silent_error)
            error.HandlerDoesNotExist
        else
            return utils.printErr(
                error.HandlerDoesNotExist,
                "Parser error: Syntax error at {f}.\n",
                .{self.source_map[self.pos]},
            );
    }

    /// Parses parameters and returns `!Node.ParameterList`. Caller is responsible for cleanup.
    pub fn parseParameters(self: *Parser) !ast.ParameterList {
        return try self.parseParametersGeneric(false);
    }

    /// Parses generic parameter list.
    /// Equivalent to a normal parameter list but the explicit type is optional.
    pub fn parseGenericParameters(self: *Parser) Error!ast.ParameterList {
        return try self.parseParametersGeneric(true);
    }

    pub fn parseArguments(self: *Parser) Error!ast.ArgumentList {
        return try self.parseArgumentsGeneric(false);
    }
    pub fn parseGenericArguments(self: *Parser) Error!ast.ArgumentList {
        return try self.parseArgumentsGeneric(true);
    }

    fn parseArgumentsGeneric(self: *Parser, comptime is_generic: bool) Error!ast.ArgumentList {
        const opening_token: lexer.Token = if (is_generic) .@"<" else .@"(";
        const closing_token: lexer.Token = if (is_generic) .@">" else .@")";
        const environment = if (is_generic) "generic argument list" else "argument list";

        var args: std.ArrayList(ast.Expression) = .empty;

        try self.expect(self.advance(), opening_token, environment, @tagName(opening_token));

        while (std.meta.activeTag(self.currentToken()) != closing_token) {
            try args.append(self.alloc, if (is_generic) b: {
                const backup_pos = self.pos;
                const current_pos = self.pos;
                if (self.type_parser.parseType(self.alloc, .default)) |t| {
                    break :b .{ .type = .{ .pos = current_pos, .payload = t } };
                } else |_| {
                    self.pos = backup_pos;
                    break :b try expressions.parse(self, .relational, .{});
                }
            } else try expressions.parse(self, .default, .{}));

            if (self.currentToken() == .@",") _ = self.advance() else break;
        }

        try self.expect(self.advance(), closing_token, environment, @tagName(closing_token));

        return args.toOwnedSlice(self.alloc);
    }

    pub fn parseBlock(self: *Parser) !ast.Block {
        var block: std.ArrayList(ast.Statement) = .empty;

        try self.expect(self.advance(), .@"{", "block", "{");

        while (self.currentToken() != .eof and self.currentToken() != .@"}")
            try block.append(self.alloc, try statements.parse(self));

        try self.expect(self.advance(), .@"}", "block", "}");

        return block.toOwnedSlice(self.alloc);
    }

    pub fn parseParametersGeneric(self: *Parser, comptime is_generic: bool) Error!ast.ParameterList {
        const context = if (is_generic) "generic parameter list" else "parameter list";
        const opening_token = if (is_generic) .@"<" else .@"(";
        const closing_token = if (is_generic) .@">" else .@")";

        var params: std.ArrayList(ast.VariableSignature) = .empty;
        try self.expect(self.advance(), opening_token, "parameter list", @tagName(opening_token));

        if (self.currentToken() == closing_token) {
            if (is_generic) return utils.printErr(
                error.UnexpectedToken,
                "Parser error: empty generic parameter list at {f}.\n",
                .{self.source_map[self.pos]},
            );

            _ = self.advance();
        } else {
            var last_arg = false;
            while (!last_arg) {
                var param_names: std.ArrayList([]const u8) = .empty;
                defer param_names.deinit(self.alloc);

                const first_pos = self.pos;

                const is_mut = if (is_generic) false else if (self.currentToken() == .mut) b: {
                    _ = self.advance();
                    break :b true;
                } else false;

                const first_name = try self.expect(self.advance(), .ident, context, "parameter name");
                try param_names.append(self.alloc, first_name);

                while (self.currentToken() == .@",") {
                    _ = self.advance();
                    const name = try self.expect(self.advance(), .ident, context, "parameter name");
                    try param_names.append(self.alloc, name);
                }

                const param_type: ast.Type = if (is_generic and self.currentToken() == .@":") b: {
                    _ = self.advance();
                    break :b try self.type_parser.parseType(self.alloc, .default);
                } else if (is_generic)
                    .{ .inferred = .{ .pos = first_pos } }
                else switch (self.advance()) {
                    .@":" => try self.type_parser.parseType(self.alloc, .default),
                    .@"..." => b: {
                        last_arg = true;
                        break :b .{ .variadic = .{ .pos = first_pos } };
                    },
                    else => |actual| return utils.printErr(
                        error.UnexpectedToken,
                        "Unexpected token '{f}' in " ++ context ++ " at {f}. Expected a type.\n",
                        .{ actual, self.source_map[self.pos - 1] },
                    ),
                };

                for (param_names.items, 0..) |p, i| try params.append(self.alloc, .{
                    .is_mut = is_mut,
                    .name = try self.alloc.dupe(u8, p),
                    .type = if (i == 0) param_type else try param_type.clone(self.alloc),
                });

                if (self.currentToken() == .@",") _ = self.advance() else {
                    try self.expect(self.advance(), closing_token, context, @tagName(closing_token));
                    break;
                }
            }
        }

        return params.toOwnedSlice(self.alloc);
    }

    pub fn parseCapture(self: *Parser) !?utils.Capture {
        return switch (self.currentToken()) {
            .@"|" => b: {
                _ = self.advance(); // consume opening pipe
                const capture_takes_ref = if (self.currentToken() == .@"&") blk: {
                    _ = self.advance();
                    break :blk true;
                } else false;
                const capture_ref_is_mut = if (capture_takes_ref and self.currentToken() == .mut) blk: {
                    _ = self.advance();
                    break :blk true;
                } else false;
                const capture_name = try self.expect(self.advance(), .ident, "capture", "capture name");

                const index: ?[]const u8 = if (self.currentToken() == .@",") b2: {
                    _ = self.advance();
                    break :b2 try self.expect(self.advance(), .ident, "capture", "index name");
                } else null;

                try self.expect(self.advance(), .@"|", "capture", "|"); // consume closing pipe
                break :b if (std.mem.eql(u8, capture_name, "_")) null else .{
                    .name = try self.alloc.dupe(u8, capture_name),
                    .takes_ref = if (capture_takes_ref) .{ .some = capture_ref_is_mut } else .none,
                    .index = if (index) |i| try self.alloc.dupe(u8, i) else null,
                };
            },
            else => null,
        };
    }

    pub inline fn expectSemicolon(self: *Parser, context: []const u8) !void {
        if (self.expect_semicolon) try self.expect(self.advance(), .@";", context, ";");
    }

    pub fn isPub(self: *Parser) bool {
        return if (self.pos <= 0) false else self.previousToken() == .@"pub";
    }
};

/// Parses `input` and returns an AST. Takes ownership and frees `input`, but *not* `source_map`.
pub fn parse(
    alloc: std.mem.Allocator,
    input: []lexer.Token,
    source_map: []utils.Position,
) !ast.RootNode {
    defer utils.deinitSlice(lexer.Token, input, alloc);

    var self: Parser = .{
        .pos = 0,
        .input = input,
        .source_map = source_map,
        .bp_lookup = .init(.{
            .@"=" = .assignment,
            .@"+=" = .assignment,
            .@"-=" = .assignment,
            .@"*=" = .assignment,
            .@"/=" = .assignment,
            .@"%=" = .assignment,
            .@"&=" = .assignment,
            .@"|=" = .assignment,
            .@"^=" = .assignment,
            .@">>=" = .assignment,
            .@"<<=" = .assignment,

            // logical
            .@"and" = .logical,
            .but = .logical,
            .@"or" = .logical,

            // relational
            .@"<" = .primary,
            .@"<=" = .relational,
            .@">" = .relational,
            .@">=" = .relational,
            .@"==" = .relational,
            .@"!=" = .relational,

            // additive & multiplicative
            .@"+" = .additive,
            .@"-" = .additive,
            .@"*" = .multiplicative,
            .@"/" = .multiplicative,
            .@"&" = .multiplicative,
            .@"%" = .multiplicative,
            .@"|" = .additive,
            .@"^" = .power,
            .@"<<" = .multiplicative,
            .@">>" = .multiplicative,
            // Call/member expressions
            .@"." = .member,
            .@"{" = .call,
            .@"(" = .call,

            // other expressions
            .@".." = .relational,
            .@"..=" = .relational,
            .@"[" = .call,
            .@"catch" = .relational,

            .int = .primary,
            .float = .primary,
            .ident = .primary,
            .string = .primary,

            // statements
            .let = .default,
            .@"const" = .default,
            .@"struct" = .default,
            .@"enum" = .default,
            .@"union" = .default,
            .@"fn" = .default,
            .bind = .default,
            .@"while" = .default,
            .@"return" = .default,
            .@"for" = .default,
            .@"if" = .default,
            .import = .default,
            .@"pub" = .default,
            .match = .default,
            .@"break" = .default,
            .@"continue" = .default,
            .@"defer" = .default,
        }),
        .nud_lookup = .init(.{
            .int = expressions.primary,
            .float = expressions.primary,
            .ident = expressions.primary,
            .string = expressions.primary,
            .@"-" = expressions.prefix,
            .@"!" = expressions.prefix,
            .@"(" = expressions.group,
            .@"fn" = expressions.functionType,
            .@"[" = expressions.arrayInstantiation,
            .@"{" = expressions.block,
            .@"if" = expressions.@"if",
            .match = expressions.match,
            .@"try" = expressions.@"try",
            .@"&" = expressions.reference,
        }),
        .led_lookup = .init(.{
            .@"=" = expressions.assignment,
            .@"+=" = expressions.assignment,
            .@"-=" = expressions.assignment,
            .@"*=" = expressions.assignment,
            .@"/=" = expressions.assignment,
            .@"%=" = expressions.assignment,
            .@"&=" = expressions.assignment,
            .@"|=" = expressions.assignment,
            .@"^=" = expressions.assignment,
            .@">>=" = expressions.assignment,
            .@"<<=" = expressions.assignment,

            // logical
            .@"and" = expressions.binary,
            .but = expressions.binary,
            .@"or" = expressions.binary,

            // relational
            .@"<" = expressions.ambiguousLessThan,
            .@"<=" = expressions.binary,
            .@">" = expressions.binary,
            .@">=" = expressions.binary,
            .@"==" = expressions.binary,
            .@"!=" = expressions.binary,

            // additive & multiplicative
            .@"+" = expressions.binary,
            .@"-" = expressions.binary,
            .@"*" = expressions.binary,
            .@"/" = expressions.binary,
            .@"&" = expressions.binary,
            .@"%" = expressions.binary,
            .@"|" = expressions.binary,
            .@"^" = expressions.binary,
            .@"<<" = expressions.binary,
            .@">>" = expressions.binary,

            // Call/member expressions
            .@"." = expressions.member,
            .@"{" = expressions.structInstantiation,
            .@"(" = expressions.call,

            // other expressions
            .@".." = expressions.range,
            .@"..=" = expressions.range,
            .@"[" = expressions.index,
            .@"catch" = expressions.@"catch",
        }),
        .statement_lookup = .init(.{
            .let = statements.variableDefinition,
            .@"const" = statements.constDefinition,
            .@"while" = statements.@"while",
            .@"return" = statements.@"return",
            .@"for" = statements.@"for",
            .@"if" = statements.@"if",
            .@"pub" = statements.@"pub",
            .match = statements.match,
            .@"break" = statements.@"break",
            .@"continue" = statements.@"continue",
            .@"defer" = statements.@"defer",
        }),
        .top_level_statement_lookup = .init(.{
            .@"enum" = top_level_statements.enumDeclaration,
            .@"struct" = top_level_statements.structDeclaration,
            .@"union" = top_level_statements.unionDeclaration,
            .@"fn" = top_level_statements.functionDefinition,
            .bind = top_level_statements.bindingDeclaration,
            .import = top_level_statements.import,
            .let = top_level_statements.variableDefinition,
            .@"const" = top_level_statements.constDefinition,
            .@"pub" = top_level_statements.@"pub",
        }),
        .type_parser = undefined,
        .alloc = alloc,
        .expect_semicolon = true,
    };
    self.type_parser = try .init(&self);

    var output: std.ArrayList(ast.TopLevelStatement) = .empty;

    while (std.meta.activeTag(self.currentToken()) != .eof)
        try output.append(self.alloc, try top_level_statements.parse(&self));

    return output.toOwnedSlice(alloc);
}

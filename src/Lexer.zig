const std = @import("std");

const utils = @import("utils");

const Lexer = struct {
    /// raw string input of source file
    input: []const u8,
    file_path: []const u8,

    /// arraylist of tokens. this is the output of the lexer.
    output: std.ArrayList(Token) = .empty,

    /// maps each token by index to its corresponding location in the source code
    source_map: std.ArrayList(utils.Position) = .empty,

    /// current position of character index in `input`
    pos: usize = 0,

    /// tracks the length of the currently tokenizing line.
    current_line_len: usize,

    /// tracks line and column number. used to report lexer errors.
    line_col: utils.Position,
    start_line_col: utils.Position,

    /// Frees resources
    pub fn deinit(self: *Lexer, alloc: std.mem.Allocator) void {
        for (self.output.items) |*i| i.deinit(alloc);
        self.output.deinit(alloc);
        self.source_map.deinit(alloc);
        alloc.destroy(self);
    }

    /// Returns characer at the current position
    inline fn currentChar(self: *const Lexer) u8 {
        return self.input[self.pos];
    }

    /// Returns current character and then increases position
    inline fn advance(self: *Lexer) u8 {
        const char = self.input[self.pos];
        self.advanceN(1);
        return char;
    }

    /// Sets `start_line_col` to current values.
    /// Called before lexing a token to save the starting position.
    /// Used to report lexer errors.
    inline fn updatePosBackup(self: *Lexer) void {
        self.start_line_col = self.line_col;
    }

    /// Skips forward `n` characters.
    fn advanceN(self: *Lexer, n: usize) void {
        self.pos += n;

        if (self.line_col.col + n <= self.current_line_len + 1) {
            self.line_col.col += n;
        } else {
            self.line_col.line += 1;
            self.line_col.col = 1;

            self.current_line_len = std.mem.indexOfScalar(
                u8,
                self.input[self.pos..],
                '\n',
            ) orelse self.input.len - self.pos;
        }
    }

    fn parseBinaryOperator(self: *Lexer, alloc: std.mem.Allocator) !void {
        self.updatePosBackup();

        const first_token_char = self.currentChar();
        if (!std.mem.containsAtLeastScalar(u8, "+-*/%=!><&|^.", 1, first_token_char)) unreachable;

        if (self.pos + 2 <= self.input.len and std.mem.eql(u8, self.input[self.pos .. self.pos + 2], "//")) {
            const end_line_pos = std.mem.indexOfScalar(u8, self.input[self.pos..], '\n') orelse
                self.input.len - self.pos;

            self.advanceN(end_line_pos);
            return;
        }

        const first_token: Token = switch (first_token_char) {
            '=' => .@"=",
            '!' => .@"!",
            '>' => .@">",
            '<' => .@"<",
            '+' => .@"+",
            '-' => .@"-",
            '*' => .@"*",
            '/' => .@"/",
            '%' => .@"%",
            '&' => .@"&",
            '|' => .@"|",
            '^' => .@"^",
            '.' => .@".",
            else => unreachable,
        };

        _ = self.advance(); // advance past first token

        const double_token: ?Token =
            if (self.pos < self.input.len) switch (self.currentChar()) {
                '=' => switch (first_token) {
                    .@"=" => .@"==",
                    .@"!" => .@"!=",
                    .@">" => .@">=",
                    .@"<" => .@"<=",
                    .@"+" => .@"+=",
                    .@"-" => .@"-=",
                    .@"*" => .@"*=",
                    .@"/" => .@"/=",
                    .@"%" => .@"%=",
                    .@"&" => .@"&=",
                    .@"|" => .@"|=",
                    .@"^" => .@"^=",
                    else => null,
                },
                '>' => switch (first_token) {
                    .@">" => .@">>",
                    .@"-" => .@"->",
                    else => null,
                },
                '<' => switch (first_token) {
                    .@"<" => .@"<<",
                    else => null,
                },
                '.' => switch (first_token) {
                    .@"." => .@"..",
                    else => null,
                },
                else => null,
            } else null;

        if (double_token) |_| {
            _ = self.advance();
        } else {
            try self.appendToken(alloc, first_token);
            return;
        }

        const triple_token: ?Token =
            if (self.pos < self.input.len) switch (self.currentChar()) {
                '=' => switch (double_token.?) {
                    .@">>" => .@">>=",
                    .@"<<" => .@"<<=",
                    .@".." => .@"..=",
                    else => null,
                },
                '.' => switch (double_token.?) {
                    .@".." => .@"...",
                    else => null,
                },
                else => null,
            } else null;

        if (triple_token) |tt| {
            _ = self.advance();
            try self.appendToken(alloc, tt);
        } else try self.appendToken(alloc, double_token.?);
    }

    /// Appends token and then advances one character. Used for readability
    inline fn appendAndNext(self: *Lexer, alloc: std.mem.Allocator, token: Token) !void {
        try self.appendToken(alloc, token);
        _ = self.advance();
    }

    fn parseNumber(self: *Lexer, alloc: std.mem.Allocator, start_pos: usize) !void {
        // Check for 0x prefix for hex literals
        if (self.currentChar() == '0' and self.pos + 1 < self.input.len and (self.input[self.pos + 1] == 'x' or self.input[self.pos + 1] == 'X')) {
            self.advanceN(2); // consume "0x"
            const hex_start_pos = self.pos;
            while (self.pos < self.input.len and std.ascii.isHex(self.currentChar())) {
                _ = self.advance();
            }

            if (self.pos == hex_start_pos) { // "0x" with no digits
                try self.appendToken(alloc, Token{ .bad_token = Error.BadNumber });
                return;
            }

            const number_str = self.input[hex_start_pos..self.pos];
            const value = std.fmt.parseInt(u64, number_str, 16) catch |err| {
                utils.print("Couldn't lex hex integer: {}", .{err}, .red);
                try self.appendToken(alloc, Token{ .bad_token = Error.BadNumber });
                return;
            };

            try self.appendToken(alloc, .{ .int = value });
            return;
        }

        var is_float = false;

        // Consume integer part
        while (self.pos < self.input.len and std.ascii.isDigit(self.currentChar())) {
            _ = self.advance();
        }

        // Check for decimal part, but look out for '..' range operator
        if (self.pos < self.input.len and self.currentChar() == '.') {
            if (self.pos + 1 < self.input.len and self.input[self.pos + 1] == '.') {
                // It's a range operator `..`, so the number part is done.
                // We don't consume the dot, we let the operator parser handle it.
            } else {
                // It's a decimal point.
                is_float = true;
                _ = self.advance(); // Consume '.'
                while (self.pos < self.input.len and std.ascii.isDigit(self.currentChar())) {
                    _ = self.advance();
                }
            }
        }

        // Check for exponent part
        if (self.pos < self.input.len and (self.currentChar() == 'e' or self.currentChar() == 'E')) {
            is_float = true;
            _ = self.advance(); // consume 'e' or 'E'

            if (self.pos < self.input.len and (self.currentChar() == '+' or self.currentChar() == '-')) {
                _ = self.advance(); // consume sign
            }

            const exponent_start = self.pos;
            while (self.pos < self.input.len and std.ascii.isDigit(self.currentChar()))
                _ = self.advance();

            if (self.pos == exponent_start) {
                // 'e' not followed by digits is an error.
                // We need to consume the 'e' and any following alphanumeric characters to avoid re-parsing.
                while (self.pos < self.input.len and std.ascii.isAlphanumeric(self.currentChar())) {
                    _ = self.advance();
                }
                try self.appendToken(alloc, Token{ .bad_token = Error.BadNumber });
                return;
            }
        }

        // A number followed by another letter is an error (e.g. `123a`).
        if (self.pos < self.input.len and std.ascii.isAlphabetic(self.input[self.pos])) {
            // Consume the rest of the bad identifier.
            while (self.pos < self.input.len and std.ascii.isAlphanumeric(self.currentChar()))
                _ = self.advance();
            try self.appendToken(alloc, Token{ .bad_token = Error.BadNumber });
            return;
        }

        const number_str = self.input[start_pos..self.pos];

        try self.appendToken(alloc, if (is_float) blk: {
            break :blk .{
                .float = std.fmt.parseFloat(f64, number_str) catch |err| {
                    utils.print("Couldn't lex float: {}\n", .{err}, .red);
                    break :blk .{ .bad_token = Error.BadNumber };
                },
            };
        } else blk: {
            break :blk .{
                .int = std.fmt.parseInt(u64, number_str, 10) catch |err| {
                    utils.print("Couldn't lex integer: {}", .{err}, .red);
                    break :blk .{ .bad_token = Error.BadNumber };
                },
            };
        });
    }

    /// Appends token to output arraylist `self.tokens` and registers the token's position in
    /// `self.source_map`
    fn appendToken(self: *Lexer, alloc: std.mem.Allocator, token: Token) !void {
        try self.output.append(alloc, token);
        try self.source_map.append(alloc, self.start_line_col);
    }
};

pub const Error = error{
    BadNumber,
    UnknownToken,
    StringNotClosed,
};

/// Type of `Token` tag.
pub const TokenKind = std.meta.Tag(Token);

/// Tagged union. Contains every kind of lexer token.
pub const Token = union(enum) {
    bad_token: Error,
    eof,

    // literals
    ident: []const u8,
    string: []const u8,
    char: u8,
    int: u64,
    float: f64,

    // special characters
    @"(",
    @")",
    @"[",
    @"]",
    @"{",
    @"}",
    @";",
    @":",
    @",",
    @".",

    // keywords
    @"and",
    @"break",
    @"catch",
    @"const",
    @"continue",
    @"defer",
    @"else",
    @"enum",
    @"fn",
    @"for",
    @"if",
    @"or",
    @"pub",
    @"return",
    @"struct",
    @"try",
    @"union",
    @"while",
    as,
    bind,
    but,
    import,
    let,
    match,
    mut,

    @"..",
    @"..=",
    @"...",
    @"->",

    // unary operators
    @"!",
    @"?",

    @"+",
    @"-",
    @"*",
    @"/",
    @"%",

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

    @"=",
    @"==",
    @">",
    @"<",
    @">=",
    @"<=",
    @"!=",

    @"&",
    @"|",
    @"^",
    @">>",
    @"<<",

    pub fn format(
        self: *const Token,
        writer: *std.Io.Writer,
    ) std.Io.Writer.Error!void {
        try switch (self.*) {
            .int => |int| writer.print("int({})", .{int}),
            .float => |float| writer.print("float({})", .{float}),
            .ident => |atom| writer.print("ident({s})", .{atom}),
            .string => |string| writer.print("string({s})", .{string}),
            .bad_token => |bad_token| writer.print("bad_token({})", .{bad_token}),
            else => |token| writer.print("{s}", .{@tagName(token)}),
        };
    }

    pub fn deinit(self: *const Token, alloc: std.mem.Allocator) void {
        switch (self.*) {
            inline .ident, .string => |str| alloc.free(str),
            else => {},
        }
    }
};

/// Initializes and runs tokenizer. Populates `tokens`. User owns return values.
pub fn tokenize(
    alloc: std.mem.Allocator,
    file_path: []const u8,
) !struct {
    []Token,
    []utils.Position,
} {
    const input = try std.fs.cwd().readFileAlloc(alloc, file_path, 1 << 20);
    defer alloc.free(input);

    const self = try alloc.create(Lexer);
    self.* = .{
        .input = input,
        .file_path = file_path,
        .current_line_len = std.mem.indexOfScalar(u8, self.input, '\n') orelse
            self.input.len,
        .line_col = .{ .line = 1, .col = 1, .path = file_path },
        .start_line_col = .{ .line = 1, .col = 1, .path = file_path },
    };
    errdefer self.output.deinit(alloc);

    while (self.pos < self.input.len) {
        const start_pos = self.pos;
        self.updatePosBackup();

        if (self.currentChar() == '"') {
            _ = self.advance(); // consume '"'

            var string: std.ArrayList(u8) = .empty;

            while (true) {
                const char = self.advance();
                if (char == '"' and (string.items.len == 0 or string.getLast() != '\\')) break;
                try string.append(alloc, char);
            }

            try self.appendToken(alloc, .{ .string = try string.toOwnedSlice(alloc) });

            continue;
        }

        if (std.ascii.isAlphabetic(self.currentChar()) or self.currentChar() == '_') {
            var atom: std.ArrayList(u8) = .empty;
            try atom.append(alloc, self.advance());

            while (self.pos < self.input.len and
                (std.ascii.isAlphanumeric(self.currentChar()) or
                    self.currentChar() == '_'))
                try atom.append(alloc, self.advance());

            const word = try atom.toOwnedSlice(alloc);

            const keyword = std.meta.stringToEnum(TokenKind, word);
            const token: Token = if (keyword) |tag| switch (tag) {
                .bad_token, .ident, .string, .char, .int, .float => unreachable,
                inline else => |t| @unionInit(Token, @tagName(t), {}),
            } else .{ .ident = word };
            try self.appendToken(alloc, token);
        } else if (std.ascii.isDigit(self.currentChar())) {
            try self.parseNumber(alloc, start_pos);
        } else switch (self.currentChar()) {
            '+', '-', '*', '/', '%', '=', '!', '>', '<', '&', '|', '^', '.' => try self.parseBinaryOperator(alloc),
            '(' => try self.appendAndNext(alloc, .@"("),
            ')' => try self.appendAndNext(alloc, .@")"),
            '[' => try self.appendAndNext(alloc, .@"["),
            ']' => try self.appendAndNext(alloc, .@"]"),
            '{' => try self.appendAndNext(alloc, .@"{"),
            '}' => try self.appendAndNext(alloc, .@"}"),
            ';' => try self.appendAndNext(alloc, .@";"),
            ':' => try self.appendAndNext(alloc, .@":"),
            ',' => try self.appendAndNext(alloc, .@","),
            '?' => try self.appendAndNext(alloc, .@"?"),
            '\'' => {
                _ = self.advance();
                try self.appendAndNext(alloc, .{ .char = self.advance() });
                _ = self.advance();
            },
            else => |char| {
                if (char == '\n' or std.ascii.isWhitespace(char)) {
                    _ = self.advance();
                    continue;
                }

                try self.appendAndNext(alloc, .{ .bad_token = Error.UnknownToken });
            },
        }
    }

    try self.appendToken(alloc, .eof);

    return .{
        try self.output.toOwnedSlice(alloc),
        try self.source_map.toOwnedSlice(alloc),
    };
}

const std = @import("std");

const utils = @import("utils");

const Self = @This();

/// raw string input of source file
input: []const u8,
file_path: []const u8,

/// arraylist of tokens. this is the output of the lexer.
tokens: std.ArrayList(Token) = .empty,

/// maps each token by index to its corresponding location in the source code
source_map: std.ArrayList(utils.Position) = .empty,

/// current position of character index in `input`
pos: usize = 0,

/// tracks the length of the currently tokenizing line.
current_line_len: usize,

/// tracks line and column number. used to report lexer errors.
line_col: utils.Position = .{ .line = 1, .col = 1 },
start_line_col: utils.Position = .{ .line = 1, .col = 1 },

pub const LexerError = error{
    BadNumber,
    UnknownToken,
    StringNotClosed,
};

/// Type of `Token` tag.
pub const TokenKind = std.meta.Tag(Token);

/// Tagged union. Contains every kind of lexer token.
pub const Token = union(enum) {
    bad_token: LexerError,
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
    @"continue",
    @"else",
    @"enum",
    @"fn",
    @"for",
    @"if",
    @"or",
    @"pub",
    @"return",
    @"struct",
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
};

/// Initializes and runs tokenizer. Populates `tokens`.
pub fn init(input: []const u8, alloc: std.mem.Allocator, file_path: []const u8) !*Self {
    const self = try alloc.create(Self);
    self.* = .{
        .input = input,
        .file_path = file_path,
        .current_line_len = std.mem.indexOfScalar(u8, self.input, '\n') orelse
            self.input.len,
        .line_col = .{ .line = 1, .col = 1, .file_path = file_path },
        .start_line_col = .{ .line = 1, .col = 1, .file_path = file_path },
    };

    try self.tokenize(alloc);

    return self;
}

/// Frees resources
pub fn deinit(self: *Self, alloc: std.mem.Allocator) void {
    self.tokens.deinit(alloc);
    self.source_map.deinit(alloc);
    alloc.destroy(self);
}

/// Returns characer at the current position
inline fn currentChar(self: *const Self) u8 {
    return self.input[self.pos];
}

/// Returns current character and then increases position
inline fn advance(self: *Self) u8 {
    const char = self.input[self.pos];
    self.advanceN(1);
    return char;
}

/// Sets `start_line_col` to current values.
/// Called before lexing a token to save the starting position.
/// Used to report lexer errors.
inline fn updatePosBackup(self: *Self) void {
    self.start_line_col = self.line_col;
}

/// Skips forward `n` characters.
fn advanceN(self: *Self, n: usize) void {
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

/// Reads `input` and populates `tokens`
pub fn tokenize(self: *Self, alloc: std.mem.Allocator) !void {
    errdefer self.tokens.deinit(alloc);

    var keywords: std.StaticStringMap(Token) = .initComptime(.{
        .{ "and", .@"and" },
        .{ "as", .as },
        .{ "bind", .bind },
        .{ "break", .@"break" },
        .{ "continue", .@"continue" },
        .{ "but", .but },
        .{ "else", .@"else" },
        .{ "enum", .@"enum" },
        .{ "fn", .@"fn" },
        .{ "for", .@"for" },
        .{ "if", .@"if" },
        .{ "import", .import },
        .{ "let", .let },
        .{ "match", .match },
        .{ "mut", .mut },
        .{ "or", .@"or" },
        .{ "pub", .@"pub" },
        .{ "return", .@"return" },
        .{ "return", .@"return" },
        .{ "struct", .@"struct" },
        .{ "union", .@"union" },
        .{ "while", .@"while" },
    });

    while (self.pos < self.input.len) {
        const start_pos = self.pos;
        self.updatePosBackup();

        if (self.currentChar() == '"') {
            _ = self.advance(); // consume '"'

            if (std.mem.indexOfScalar(u8, self.input[self.pos..], '"')) |string_end| {
                try self.appendToken(alloc, Token{ .string = self.input[self.pos .. self.pos + string_end] });
                self.advanceN(string_end + 1); // move forward and also consume closing quote
            } else {
                try self.appendToken(alloc, .{ .bad_token = error.StringNotClosed });
            }

            continue;
        }

        if (std.ascii.isAlphabetic(self.currentChar()) or self.currentChar() == '_') {
            var atom = std.ArrayList(u8){};
            try atom.append(alloc, self.advance());

            while (self.pos < self.input.len and
                (std.ascii.isAlphanumeric(self.currentChar()) or
                    self.currentChar() == '_'))
                try atom.append(alloc, self.advance());

            const word = try atom.toOwnedSlice(alloc);

            // TODO: try to replace with std.meta.stringToEnum(comptime T: type, str: []const u8)
            const token: Token = keywords.get(word) orelse .{ .ident = word };
            try self.appendToken(alloc, token);
        } else if (std.ascii.isDigit(self.currentChar())) {
            try self.parseNumber(alloc, start_pos);
        } else {
            const char = self.currentChar();
            switch (char) {
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
                else => {
                    if (char == '\n' or std.ascii.isWhitespace(char)) {
                        _ = self.advance();
                        continue;
                    }

                    try self.appendAndNext(alloc, .{ .bad_token = LexerError.UnknownToken });
                },
            }
        }
    }

    try self.appendToken(alloc, .eof);
}

fn parseBinaryOperator(self: *Self, alloc: std.mem.Allocator) !void {
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

    _ = self.advance();
    const double_token: Token =
        if (self.pos < self.input.len) switch (self.currentChar()) {
            '=' => blk: {
                _ = self.advance();
                break :blk switch (first_token) {
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
                    else => unreachable,
                };
            },
            '>' => blk: {
                _ = self.advance();
                break :blk switch (first_token) {
                    .@">" => .@">>",
                    .@"-" => .@"->",
                    else => first_token,
                };
            },
            '<' => blk: {
                _ = self.advance();
                break :blk switch (first_token) {
                    .@"<" => .@"<<",
                    else => first_token,
                };
            },
            '.' => blk: {
                _ = self.advance();
                break :blk switch (first_token) {
                    .@"." => .@"..",
                    else => first_token,
                };
            },
            else => first_token,
        } else first_token;

    const triple_token: Token =
        if (self.pos < self.input.len) switch (self.currentChar()) {
            '=' => blk: {
                _ = self.advance();
                break :blk switch (double_token) {
                    .@">>" => .@">>=",
                    .@"<<" => .@"<<=",
                    .@".." => .@"..=",
                    else => double_token,
                };
            },
            '.' => blk: {
                _ = self.advance();
                break :blk switch (double_token) {
                    .@".." => .@"...",
                    else => double_token,
                };
            },
            else => double_token,
        } else double_token;

    try self.appendToken(alloc, triple_token);
}

/// Appends token and then advances one character. Used for readability
inline fn appendAndNext(self: *Self, alloc: std.mem.Allocator, token: Token) !void {
    try self.appendToken(alloc, token);
    _ = self.advance();
}

fn parseNumber(self: *Self, alloc: std.mem.Allocator, start_pos: usize) !void {
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
            try self.appendToken(alloc, Token{ .bad_token = LexerError.BadNumber });
            return;
        }
    }

    // A number followed by another letter is an error (e.g. `123a`).
    if (self.pos < self.input.len and std.ascii.isAlphabetic(self.input[self.pos])) {
        // Consume the rest of the bad identifier.
        while (self.pos < self.input.len and std.ascii.isAlphanumeric(self.currentChar()))
            _ = self.advance();
        try self.appendToken(alloc, Token{ .bad_token = LexerError.BadNumber });
        return;
    }

    const number_str = self.input[start_pos..self.pos];

    try self.appendToken(alloc, if (is_float) blk: {
        break :blk .{
            .float = std.fmt.parseFloat(f64, number_str) catch |err| {
                utils.print("Couldn't lex float: {}\n", .{err}, .red);
                break :blk .{ .bad_token = LexerError.BadNumber };
            },
        };
    } else blk: {
        break :blk .{
            .int = std.fmt.parseInt(u64, number_str, 10) catch |err| {
                utils.print("Couldn't lex integer: {}", .{err}, .red);
                break :blk .{ .bad_token = LexerError.BadNumber };
            },
        };
    });
}

/// Appends token to output arraylist `self.tokens` and registers the token's position in
/// `self.source_map`
fn appendToken(self: *Self, alloc: std.mem.Allocator, token: Token) !void {
    try self.tokens.append(alloc, token);
    try self.source_map.append(alloc, self.start_line_col);
}

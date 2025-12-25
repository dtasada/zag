const std = @import("std");

const utils = @import("utils.zig");

const Self = @This();

/// raw string input of source file
source_code: []const u8,

/// arraylist of tokens. this is the output of the lexer.
tokens: std.ArrayList(Token) = .empty,

/// maps each token by index to its corresponding location in the source code
source_map: std.ArrayList(utils.Position) = .empty,

/// current position of character index in `input`
pos: usize = 0,

/// tracks line and column number
line: usize = 1,
column: usize = 1,

const LexerError = error{
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
    open_paren,
    close_paren,
    open_bracket,
    close_bracket,
    open_brace,
    close_brace,
    semicolon,
    colon,
    comma,
    dot,

    // keywords
    let,
    mut,
    @"struct",
    @"enum",
    @"union",
    import,
    @"fn",
    @"if",
    @"else",
    @"for",
    @"while",
    in,
    @"return",
    @"and",
    @"or",
    @"pub",

    dot_dot,
    dot_dot_equals,

    // unary operators
    bang,
    question,

    plus,
    dash,
    asterisk,
    slash,
    percent,

    plus_equals,
    minus_equals,
    times_equals,
    slash_equals,
    mod_equals,
    and_equals,
    or_equals,
    xor_equals,
    shift_right_equals,
    shift_left_equals,

    equals,
    equals_equals,
    greater,
    less,
    greater_equals,
    less_equals,
    bang_equals,

    ampersand,
    pipe,
    caret,
    shift_right,
    shift_left,
    logical_and,
    logical_or,

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
pub fn init(input: []const u8, alloc: std.mem.Allocator) !*Self {
    const self = try alloc.create(Self);
    self.* = .{ .source_code = input };

    try self.tokenize(alloc);

    return self;
}

pub fn deinit(self: *Self, alloc: std.mem.Allocator) void {
    self.tokens.deinit(alloc);
    self.source_map.deinit(alloc);
    alloc.destroy(self);
}

inline fn currentChar(self: *const Self) u8 {
    return self.source_code[self.pos];
}

/// Returns current character and then increases position
inline fn advance(self: *Self) u8 {
    const char = self.source_code[self.pos];
    self.advanceN(1);
    return char;
}

inline fn advanceN(self: *Self, n: usize) void {
    self.column += n;
    self.pos += n;
}

/// Reads `input` and populates `tokens`
pub fn tokenize(self: *Self, alloc: std.mem.Allocator) !void {
    errdefer self.tokens.deinit(alloc);

    var keywords = std.StringHashMap(Token).init(alloc);
    try keywords.put("let", .let);
    try keywords.put("mut", .mut);
    try keywords.put("struct", .@"struct");
    try keywords.put("enum", .@"enum");
    try keywords.put("union", .@"union");
    try keywords.put("import", .import);
    try keywords.put("fn", .@"fn");
    try keywords.put("if", .@"if");
    try keywords.put("else", .@"else");
    try keywords.put("for", .@"for");
    try keywords.put("while", .@"while");
    try keywords.put("in", .in);
    try keywords.put("return", .@"return");
    try keywords.put("and", .@"and");
    try keywords.put("or", .@"or");
    try keywords.put("pub", .@"pub");

    while (self.pos < self.source_code.len) {
        const start_pos = self.pos;

        if (self.currentChar() == '"') {
            _ = self.advance(); // consume '"'

            if (std.mem.indexOfScalar(u8, self.source_code[self.pos..], '"')) |string_end| {
                try self.appendToken(alloc, Token{ .string = self.source_code[self.pos .. self.pos + string_end] });
                self.advanceN(string_end + 1); // move forward and also consume closing quote
            } else {
                try self.appendToken(alloc, .{ .bad_token = error.StringNotClosed });
            }

            continue;
        }

        if (std.ascii.isAlphabetic(self.currentChar()) or self.currentChar() == '_') {
            var atom = std.ArrayList(u8){};
            try atom.append(alloc, self.advance());

            while (self.pos < self.source_code.len and
                (std.ascii.isAlphanumeric(self.currentChar()) or
                    self.currentChar() == '_'))
                try atom.append(alloc, self.advance());

            const word = try atom.toOwnedSlice(alloc);

            const token: Token = keywords.get(word) orelse .{ .ident = word };
            try self.appendToken(alloc, token);
        } else if (std.ascii.isDigit(self.currentChar())) {
            try self.parseNumber(alloc, start_pos);
        } else {
            const char = self.currentChar();
            switch (char) {
                '+', '-', '*', '/', '%', '=', '!', '>', '<', '&', '|', '^', '.' => try self.parseBinaryOperator(alloc),
                '(' => try self.appendAndNext(alloc, .open_paren),
                ')' => try self.appendAndNext(alloc, .close_paren),
                '[' => try self.appendAndNext(alloc, .open_bracket),
                ']' => try self.appendAndNext(alloc, .close_bracket),
                '{' => try self.appendAndNext(alloc, .open_brace),
                '}' => try self.appendAndNext(alloc, .close_brace),
                ';' => try self.appendAndNext(alloc, .semicolon),
                ':' => try self.appendAndNext(alloc, .colon),
                ',' => try self.appendAndNext(alloc, .comma),
                '?' => try self.appendAndNext(alloc, .question),
                '\'' => {
                    _ = self.advance();
                    try self.appendAndNext(alloc, .{ .char = self.advance() });
                    _ = self.advance();
                },
                else => {
                    if (char == '\n') {
                        self.line += 1;
                        self.column = 1;
                    }

                    if (std.ascii.isWhitespace(char)) {
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
    const first_token_char = self.currentChar();
    if (!std.mem.containsAtLeastScalar(u8, "+-*/%=!><&|^.", 1, first_token_char)) unreachable;

    if (self.pos + 2 <= self.source_code.len and std.mem.eql(u8, self.source_code[self.pos .. self.pos + 2], "//")) {
        const end_line_pos = std.mem.indexOfScalar(u8, self.source_code[self.pos..], '\n') orelse
            std.mem.indexOfScalar(u8, self.source_code[self.pos..], '\n') orelse
            self.source_code.len - self.pos;

        self.advanceN(end_line_pos);
        return;
    }

    const first_token: Token = switch (first_token_char) {
        '=' => .equals,
        '!' => .bang,
        '>' => .greater,
        '<' => .less,
        '+' => .plus,
        '-' => .dash,
        '*' => .asterisk,
        '/' => .slash,
        '%' => .percent,
        '&' => .ampersand,
        '|' => .pipe,
        '^' => .caret,
        '.' => .dot,
        else => unreachable,
    };

    _ = self.advance();
    const double_token: Token =
        if (self.pos < self.source_code.len) switch (self.currentChar()) {
            '=' => blk: {
                _ = self.advance();
                break :blk switch (first_token) {
                    .equals => .equals_equals,
                    .bang => .bang_equals,
                    .greater => .greater_equals,
                    .less => .less_equals,
                    .plus => .plus_equals,
                    .dash => .minus_equals,
                    .asterisk => .times_equals,
                    .slash => .slash_equals,
                    .percent => .mod_equals,
                    .ampersand => .and_equals,
                    .pipe => .or_equals,
                    .caret => .xor_equals,
                    else => unreachable,
                };
            },
            '>' => blk: {
                _ = self.advance();
                break :blk switch (first_token) {
                    .greater => .shift_right,
                    else => first_token,
                };
            },
            '<' => blk: {
                _ = self.advance();
                break :blk switch (first_token) {
                    .less => .shift_left,
                    else => first_token,
                };
            },
            '.' => blk: {
                _ = self.advance();
                break :blk switch (first_token) {
                    .dot => .dot_dot,
                    else => first_token,
                };
            },
            else => first_token,
        } else first_token;

    const triple_token: Token =
        if (self.pos < self.source_code.len) switch (self.currentChar()) {
            '=' => blk: {
                _ = self.advance();
                break :blk switch (double_token) {
                    .shift_right => .shift_right_equals,
                    .shift_left => .shift_left_equals,
                    else => double_token,
                };
            },
            '.' => blk: {
                _ = self.advance();
                break :blk switch (double_token) {
                    .dot_dot => .dot_dot_equals,
                    else => double_token,
                };
            },
            else => double_token,
        } else double_token;

    try self.appendToken(alloc, triple_token);
}

inline fn appendAndNext(self: *Self, alloc: std.mem.Allocator, token: Token) !void {
    try self.appendToken(alloc, token);
    _ = self.advance();
}

fn parseNumber(self: *Self, alloc: std.mem.Allocator, start_pos: usize) !void {
    var is_float = false;

    // Consume integer part
    while (self.pos < self.source_code.len and std.ascii.isDigit(self.currentChar())) {
        _ = self.advance();
    }

    // Check for decimal part, but look out for '..' range operator
    if (self.pos < self.source_code.len and self.currentChar() == '.') {
        if (self.pos + 1 < self.source_code.len and self.source_code[self.pos + 1] == '.') {
            // It's a range operator `..`, so the number part is done.
            // We don't consume the dot, we let the operator parser handle it.
        } else {
            // It's a decimal point.
            is_float = true;
            _ = self.advance(); // Consume '.'
            while (self.pos < self.source_code.len and std.ascii.isDigit(self.currentChar())) {
                _ = self.advance();
            }
        }
    }

    // Check for exponent part
    if (self.pos < self.source_code.len and (self.currentChar() == 'e' or self.currentChar() == 'E')) {
        is_float = true;
        _ = self.advance(); // consume 'e' or 'E'

        if (self.pos < self.source_code.len and (self.currentChar() == '+' or self.currentChar() == '-')) {
            _ = self.advance(); // consume sign
        }

        const exponent_start = self.pos;
        while (self.pos < self.source_code.len and std.ascii.isDigit(self.currentChar())) {
            _ = self.advance();
        }
        if (self.pos == exponent_start) {
            // 'e' not followed by digits is an error.
            // We need to consume the 'e' and any following alphanumeric characters to avoid re-parsing.
            while (self.pos < self.source_code.len and std.ascii.isAlphanumeric(self.currentChar())) {
                _ = self.advance();
            }
            try self.appendToken(alloc, Token{ .bad_token = LexerError.BadNumber });
            return;
        }
    }

    // A number followed by another letter is an error (e.g. `123a`).
    if (self.pos < self.source_code.len and std.ascii.isAlphabetic(self.source_code[self.pos])) {
        // Consume the rest of the bad identifier.
        while (self.pos < self.source_code.len and std.ascii.isAlphanumeric(self.currentChar())) {
            _ = self.advance();
        }
        try self.appendToken(alloc, Token{ .bad_token = LexerError.BadNumber });
        return;
    }

    const number_str = self.source_code[start_pos..self.pos];

    if (is_float) {
        const token = blk: {
            const num = std.fmt.parseFloat(f64, number_str) catch |err| {
                utils.print("Couldn't parse float: {}\n", .{err}, .red);
                break :blk Token{ .bad_token = LexerError.BadNumber };
            };
            break :blk Token{ .float = num };
        };
        try self.appendToken(alloc, token);
    } else {
        const token = blk: {
            const num = std.fmt.parseInt(u64, number_str, 10) catch |err| {
                utils.print("Couldn't parse integer: {}", .{err}, .red);
                break :blk Token{ .bad_token = LexerError.BadNumber };
            };
            break :blk Token{ .int = num };
        };
        try self.appendToken(alloc, token);
    }
}

fn appendToken(self: *Self, alloc: std.mem.Allocator, token: Token) !void {
    try self.tokens.append(alloc, token);
    try self.source_map.append(alloc, .{ .line = self.line, .col = self.column });
}

const std = @import("std");

const utils = @import("utils.zig");

const Self = @This();

/// raw string input of source file
input: []const u8,

/// arraylist of tokens. this is the output of the lexer.
tokens: std.ArrayList(Token) = .{},

/// maps each token by index to its corresponding location in the source code
source_map: std.ArrayList(struct { line: usize, col: usize }) = .{},

/// current position of character index in `input`
pos: usize,

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
    @"var",
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

    // unary operators
    bang,

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

pub fn init(input: []const u8) Self {
    return .{
        .input = input,
        .pos = 0,
    };
}

pub fn deinit(self: *Self, alloc: std.mem.Allocator) void {
    self.tokens.deinit(alloc);
    self.source_map.deinit(alloc);
}

inline fn currentChar(self: *const Self) u8 {
    return self.input[self.pos];
}

/// Returns current character and then increases position
inline fn consumeChar(self: *Self) u8 {
    const char = self.input[self.pos];
    self.pos += 1;
    return char;
}

/// Reads `input` and populates `tokens`
pub fn tokenize(self: *Self, alloc: std.mem.Allocator) !void {
    errdefer self.tokens.deinit(alloc);

    while (self.pos < self.input.len) {
        const start_pos = self.pos;
        if (self.currentChar() == '"') {
            self.pos += 1;

            if (std.mem.indexOfScalar(u8, self.input[self.pos..], '"')) |string_end| {
                try self.appendToken(alloc, Token{ .string = self.input[self.pos .. self.pos + string_end] }, start_pos);
                self.pos += string_end + 1; // move forward and also consume closing quote
            } else {
                try self.appendToken(alloc, .{ .bad_token = error.StringNotClosed }, start_pos);
            }

            continue;
        }

        if (std.ascii.isAlphabetic(self.currentChar()) or self.currentChar() == '_') {
            var atom: [128]u8 = undefined;
            atom[0] = self.consumeChar();

            var i: usize = 1;
            while (self.pos < self.input.len and
                (std.ascii.isAlphanumeric(self.currentChar()) or
                    self.currentChar() == '_')) : (i += 1)
                atom[i] = self.consumeChar();

            const word = atom[0..i];

            const token: Token =
                if (std.mem.eql(u8, word, "let"))
                    .let
                else if (std.mem.eql(u8, word, "var"))
                    .@"var"
                else if (std.mem.eql(u8, word, "struct"))
                    .@"struct"
                else if (std.mem.eql(u8, word, "enum"))
                    .@"enum"
                else if (std.mem.eql(u8, word, "union"))
                    .@"union"
                else if (std.mem.eql(u8, word, "import"))
                    .import
                else if (std.mem.eql(u8, word, "fn"))
                    .@"fn"
                else if (std.mem.eql(u8, word, "if"))
                    .@"if"
                else if (std.mem.eql(u8, word, "else"))
                    .@"else"
                else if (std.mem.eql(u8, word, "for"))
                    .@"for"
                else if (std.mem.eql(u8, word, "while"))
                    .@"while"
                else if (std.mem.eql(u8, word, "in"))
                    .in
                else if (std.mem.eql(u8, word, "return"))
                    .@"return"
                else if (std.mem.eql(u8, word, "and"))
                    .@"and"
                else if (std.mem.eql(u8, word, "or"))
                    .@"or"
                else if (std.mem.eql(u8, word, "pub"))
                    .@"pub"
                else
                    .{ .ident = try alloc.dupe(u8, word) };

            try self.appendToken(alloc, token, start_pos);
        } else if (std.ascii.isDigit(self.currentChar())) {
            try self.parseNumber(alloc, start_pos);
        } else {
            const char = self.currentChar();
            const non_alphanumeric = "+-*/()[]{};:,=!><&|^.";

            // if char is a valid non-alphanumeric character
            if (std.mem.containsAtLeastScalar(u8, non_alphanumeric, 1, char)) {
                switch (char) {
                    '+', '-', '*', '/', '%', '=', '!', '>', '<', '&', '|', '^' => try self.parseBinaryOperator(alloc, start_pos),
                    '(' => try self.appendAndNext(alloc, .open_paren),
                    ')' => try self.appendAndNext(alloc, .close_paren),
                    '[' => try self.appendAndNext(alloc, .open_bracket),
                    ']' => try self.appendAndNext(alloc, .close_bracket),
                    '{' => try self.appendAndNext(alloc, .open_brace),
                    '}' => try self.appendAndNext(alloc, .close_brace),
                    ';' => try self.appendAndNext(alloc, .semicolon),
                    ':' => try self.appendAndNext(alloc, .colon),
                    ',' => try self.appendAndNext(alloc, .comma),
                    '.' => try self.appendAndNext(alloc, .dot),
                    else => unreachable,
                }
            } else {
                if (std.ascii.isWhitespace(char)) {
                    self.pos += 1;
                    continue;
                }
                if (std.ascii.isAlphanumeric(char))
                    unreachable
                else
                    try self.appendAndNext(alloc, .{ .bad_token = LexerError.UnknownToken });
            }
        }
    }

    try self.appendToken(alloc, .eof, self.pos);
}

fn parseBinaryOperator(self: *Self, alloc: std.mem.Allocator, start_pos: usize) !void {
    const first_token_char = self.currentChar();
    if (!std.mem.containsAtLeastScalar(u8, "+-*/%=!><&|^", 1, first_token_char)) unreachable;

    if (self.pos + 2 <= self.input.len and std.mem.eql(u8, self.input[self.pos .. self.pos + 2], "//")) {
        const end_line_pos = std.mem.indexOfScalar(u8, self.input[self.pos..], '\n') orelse
            std.mem.indexOfScalar(u8, self.input[self.pos..], '\n') orelse
            self.input.len - self.pos;

        self.pos += end_line_pos;
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
        else => unreachable,
    };

    self.pos += 1;
    const double_token: Token =
        if (self.pos < self.input.len) switch (self.currentChar()) {
            '=' => blk: {
                self.pos += 1;
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
                self.pos += 1;
                break :blk switch (first_token) {
                    .greater => .shift_right,
                    else => first_token,
                };
            },
            '<' => blk: {
                self.pos += 1;
                break :blk switch (first_token) {
                    .less => .shift_left,
                    else => first_token,
                };
            },
            else => first_token,
        } else first_token;

    const triple_token: Token =
        if (self.pos < self.input.len) switch (self.currentChar()) {
            '=' => blk: {
                self.pos += 1;
                break :blk switch (double_token) {
                    .shift_right => .shift_right_equals,
                    .shift_left => .shift_left_equals,
                    else => double_token,
                };
            },
            else => double_token,
        } else double_token;

    try self.appendToken(alloc, triple_token, start_pos);
}

inline fn appendAndNext(self: *Self, alloc: std.mem.Allocator, token: Token) !void {
    try self.appendToken(alloc, token, self.pos);
    self.pos += 1;
}

fn parseNumber(self: *Self, alloc: std.mem.Allocator, start_pos: usize) !void {
    var passed_decimal = false;

    while (self.pos < self.input.len and (std.ascii.isDigit(self.currentChar()) or self.currentChar() == '.')) {
        if (self.currentChar() == '.') {
            if (passed_decimal) {
                try self.appendToken(alloc, .{ .bad_token = LexerError.BadNumber }, start_pos);
                return;
            }
            passed_decimal = true;
        }
        self.pos += 1;
    }

    if (self.pos < self.input.len and std.ascii.isAlphabetic(self.input[self.pos])) {
        while (self.pos < self.input.len and std.ascii.isAlphanumeric(self.currentChar())) {
            self.pos += 1;
        }
        try self.appendToken(alloc, Token{ .bad_token = LexerError.BadNumber }, start_pos);
        return;
    }

    if (passed_decimal) {
        const token = blk: {
            const num = std.fmt.parseFloat(f64, self.input[start_pos..self.pos]) catch |err| {
                utils.print("Couldn't parse float: {}\n", .{err}, .red);
                break :blk Token{ .bad_token = LexerError.BadNumber };
            };
            break :blk Token{ .float = num };
        };
        try self.appendToken(alloc, token, start_pos);
    } else {
        const token = blk: {
            const num = std.fmt.parseInt(u64, self.input[start_pos..self.pos], 10) catch |err| {
                utils.print("Couldn't parse integer: {}", .{err}, .red);
                break :blk Token{ .bad_token = LexerError.BadNumber };
            };
            break :blk Token{ .int = num };
        };
        try self.appendToken(alloc, token, start_pos);
    }
}

fn appendToken(self: *Self, alloc: std.mem.Allocator, token: Token, start_pos: usize) !void {
    try self.tokens.append(alloc, token);

    var report_pos = start_pos;
    if (token == .eof) {
        while (report_pos > 0 and std.ascii.isWhitespace(self.input[report_pos - 1]))
            report_pos -= 1;
    }

    const processed_input = self.input[0..report_pos];
    const amount_of_lines = std.mem.count(u8, processed_input, "\n");
    const last_newline_pos = std.mem.lastIndexOfScalar(u8, processed_input, '\n');

    const line = amount_of_lines + 1;
    const col = if (last_newline_pos) |p|
        report_pos - p
    else
        report_pos + 1;

    try self.source_map.append(alloc, .{ .line = line, .col = col });
}

import re

new_content = """fn isGenericLookahead(self: *Self) bool {
    var depth: usize = 0;
    var i: usize = self.pos;

    while (i < self.lexer.tokens.items.len) : (i += 1) {
        switch (self.lexer.tokens.items[i]) {
            .@"<" => depth += 1,
            .@">" => {
                depth -= 1;
                if (depth == 0) {
                    if (i + 1 < self.lexer.tokens.items.len) {
                        return self.lexer.tokens.items[i + 1] == .@"(";
                    }
                    return false;
                }
            },
            .@"(", .@")", .@"{", .@"}", .@";" => return false,
            else => {},
        }
    }

    return false;
}"""

with open('src/parser/expressions.zig', 'r') as f:
    content = f.read()

# Find the old isGenericLookahead function using regex to be more robust
# in case of minor whitespace differences.
# This regex looks for 'fn isGenericLookahead(self: *Self) bool {'
# then captures everything until the closing '}' of the function.
# It uses a non-greedy match for the content between the braces.
match = re.search(r'(fn isGenericLookahead(self: *Self) bool {\n(?:.*\n)*?})', content)

if match:
    old_function_block = match.group(1)
    # Replace the matched old function block with the new content
    modified_content = content.replace(old_function_block, new_content)
    with open('src/parser/expressions.zig', 'w') as f:
        f.write(modified_content)
    print("Successfully updated isGenericLookahead in src/parser/expressions.zig")
else:
    print("Error: Could not find the old isGenericLookahead function in src/parser/expressions.zig")

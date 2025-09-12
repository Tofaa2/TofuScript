const std = @import("std");
const tokens = @import("tokens.zig");

const Token = tokens.Token;
const TokenType = tokens.TokenType;

pub const Lexer = struct {
    source: []const u8,
    start: usize = 0,
    current: usize = 0,
    line: usize = 1,
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator, source: []const u8) Lexer {
        return Lexer{
            .source = source,
            .allocator = allocator,
        };
    }

    pub fn scanTokens(self: *Lexer) !std.ArrayList(Token) {
        var token_list = std.ArrayList(Token).init(self.allocator);

        while (!self.isAtEnd()) {
            self.start = self.current;
            try self.scanToken(&token_list);
        }

        try token_list.append(try Token.init(self.allocator, TokenType.eof, "", self.line));

        return token_list;
    }

    fn scanToken(self: *Lexer, token_list: *std.ArrayList(Token)) !void {
        const c = self.advance();

        switch (c) {
            '(' => try self.addToken(token_list, TokenType.left_paren),
            ')' => try self.addToken(token_list, TokenType.right_paren),
            '{' => try self.addToken(token_list, TokenType.left_brace),
            '}' => try self.addToken(token_list, TokenType.right_brace),
            ',' => try self.addToken(token_list, TokenType.comma),
            '.' => try self.addToken(token_list, TokenType.dot),
            '-' => try self.addToken(token_list, TokenType.minus),
            '+' => try self.addToken(token_list, TokenType.plus),
            ';' => try self.addToken(token_list, TokenType.semicolon),
            '*' => try self.addToken(token_list, TokenType.star),
            ':' => try self.addToken(token_list, TokenType.colon),
            '!' => try self.addToken(token_list, if (self.match('=')) TokenType.bang_equal else TokenType.bang),
            '=' => try self.addToken(token_list, if (self.match('=')) TokenType.equal_equal else TokenType.equal),
            '<' => try self.addToken(token_list, if (self.match('=')) TokenType.less_equal else TokenType.less),
            '>' => try self.addToken(token_list, if (self.match('=')) TokenType.greater_equal else TokenType.greater),
            '/' => {
                if (self.match('/')) {
                    // A comment goes until the end of the line
                    while (self.peek() != '\n' and !self.isAtEnd()) {
                        _ = self.advance();
                    }
                } else {
                    try self.addToken(token_list, TokenType.slash);
                }
            },
            ' ', '\r', '\t' => {}, // Ignore whitespace
            '\n' => self.line += 1,
            '"' => try self.string(token_list),
            '0'...'9' => try self.number(token_list),
            'a'...'z', 'A'...'Z', '_' => try self.identifier(token_list),
            else => {
                std.debug.print("Unexpected character: {c}\n", .{c});
            },
        }
    }

    fn identifier(self: *Lexer, token_list: *std.ArrayList(Token)) !void {
        while (isAlphaNumeric(self.peek())) {
            _ = self.advance();
        }

        const text = self.source[self.start..self.current];
        const token_type =
            if (std.mem.eql(u8, text, "func")) TokenType.func else if (std.mem.eql(u8, text, "var")) TokenType.@"var" else if (std.mem.eql(u8, text, "if")) TokenType.@"if" else if (std.mem.eql(u8, text, "else")) TokenType.@"else" else if (std.mem.eql(u8, text, "return")) TokenType.@"return" else if (std.mem.eql(u8, text, "true")) TokenType.true else if (std.mem.eql(u8, text, "false")) TokenType.false else if (std.mem.eql(u8, text, "nil")) TokenType.nil else if (std.mem.eql(u8, text, "and")) TokenType.@"and" else if (std.mem.eql(u8, text, "or")) TokenType.@"or" else if (std.mem.eql(u8, text, "while")) TokenType.@"while" else if (std.mem.eql(u8, text, "for")) TokenType.@"for" else if (std.mem.eql(u8, text, "break")) TokenType.@"break" else if (std.mem.eql(u8, text, "continue")) TokenType.@"continue" else if (std.mem.eql(u8, text, "print")) TokenType.print else if (std.mem.eql(u8, text, "import")) TokenType.import else if (std.mem.eql(u8, text, "struct")) TokenType.@"struct" else if (std.mem.eql(u8, text, "trait")) TokenType.trait else if (std.mem.eql(u8, text, "impl")) TokenType.impl else if (std.mem.eql(u8, text, "as")) TokenType.as else TokenType.identifier;

        try self.addToken(token_list, token_type);
    }

    fn number(self: *Lexer, token_list: *std.ArrayList(Token)) !void {
        while (isDigit(self.peek())) {
            _ = self.advance();
        }

        // Look for a fractional part
        if (self.peek() == '.' and isDigit(self.peekNext())) {
            // Consume the "."
            _ = self.advance();

            while (isDigit(self.peek())) {
                _ = self.advance();
            }
        }

        try self.addToken(token_list, TokenType.number);
    }

    fn string(self: *Lexer, token_list: *std.ArrayList(Token)) !void {
        while (self.peek() != '"' and !self.isAtEnd()) {
            if (self.peek() == '\n') {
                self.line += 1;
            }
            _ = self.advance();
        }

        if (self.isAtEnd()) {
            std.debug.print("Unterminated string.\n", .{});
            return;
        }

        // The closing "
        _ = self.advance();

        // Trim the surrounding quotes
        const value = self.source[self.start + 1 .. self.current - 1];
        _ = value;
        try self.addToken(token_list, TokenType.string);
    }

    fn addToken(self: *Lexer, token_list: *std.ArrayList(Token), token_type: TokenType) !void {
        const text = self.source[self.start..self.current];
        try token_list.append(try Token.init(self.allocator, token_type, text, self.line));
    }

    fn match(self: *Lexer, expected: u8) bool {
        if (self.isAtEnd()) {
            return false;
        }
        if (self.source[self.current] != expected) {
            return false;
        }

        self.current += 1;
        return true;
    }

    fn peek(self: *Lexer) u8 {
        if (self.isAtEnd()) {
            return 0;
        }
        return self.source[self.current];
    }

    fn peekNext(self: *Lexer) u8 {
        if (self.current + 1 >= self.source.len) {
            return 0;
        }
        return self.source[self.current + 1];
    }

    fn isAlpha(self: u8) bool {
        return (self >= 'a' and self <= 'z') or
            (self >= 'A' and self <= 'Z') or
            self == '_';
    }

    fn isAlphaNumeric(self: u8) bool {
        return isAlpha(self) or isDigit(self);
    }

    fn isDigit(self: u8) bool {
        return self >= '0' and self <= '9';
    }

    fn advance(self: *Lexer) u8 {
        self.current += 1;
        return self.source[self.current - 1];
    }

    fn isAtEnd(self: *Lexer) bool {
        return self.current >= self.source.len;
    }
};

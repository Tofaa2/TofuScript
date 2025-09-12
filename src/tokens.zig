const std = @import("std");

pub const TokenType = enum {
    // Single-character tokens
    left_paren,
    right_paren,
    left_brace,
    right_brace,
    comma,
    dot,
    minus,
    plus,
    semicolon,
    slash,
    star,
    colon,

    // One or two character tokens
    bang,
    bang_equal,
    equal,
    equal_equal,
    greater,
    greater_equal,
    less,
    less_equal,

    // Literals
    identifier,
    string,
    number,

    // Keywords
    @"and",
    class,
    @"else",
    false,
    @"for",
    func,
    @"if",
    nil,
    @"or",
    print,
    @"return",
    @"break",
    @"continue",
    super,
    this,
    true,
    @"var",
    @"while",
    import,
    @"struct",
    trait,
    impl,
    as,

    eof,
};

pub const Token = struct {
    type: TokenType,
    lexeme: []const u8,
    line: usize,

    pub fn init(allocator: std.mem.Allocator, tok_type: TokenType, lexeme: []const u8, line: usize) !Token {
        const copied_lexeme = try allocator.dupe(u8, lexeme);
        return Token{
            .type = tok_type,
            .lexeme = copied_lexeme,
            .line = line,
        };
    }

    pub fn deinit(self: Token, allocator: std.mem.Allocator) void {
        allocator.free(self.lexeme);
    }
};

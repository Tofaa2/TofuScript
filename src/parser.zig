const std = @import("std");
const tokens = @import("tokens.zig");
const ast = @import("ast.zig");

const Token = tokens.Token;
const TokenType = tokens.TokenType;
const Node = ast.Node;
const NodeType = ast.NodeType;
const LiteralValue = ast.LiteralValue;

pub const Parser = struct {
    tokens: std.ArrayList(Token),
    current: usize = 0,
    allocator: std.mem.Allocator,
    had_error: bool = false,
    panic_mode: bool = false,

    pub fn init(allocator: std.mem.Allocator, toks: std.ArrayList(Token)) Parser {
        return Parser{
            .tokens = toks,
            .allocator = allocator,
        };
    }

    pub fn parse(self: *Parser) !*Node {
        const program = try Node.init(self.allocator, .program);

        while (!self.isAtEnd()) {
            const stmt = self.declaration() catch |err| {
                if (err == error.ParseError) {
                    self.synchronize();
                    continue;
                } else {
                    return err;
                }
            };
            try program.data.program.body.append(stmt);
        }

        return program;
    }

    fn declaration(self: *Parser) !*Node {
        if (self.match(.func)) {
            return try self.functionDeclaration();
        }
        if (self.match(.import)) {
            return try self.importStatement();
        }
        if (self.match(.@"var")) {
            return try self.varDeclaration();
        }

        return try self.statement();
    }

    fn functionDeclaration(self: *Parser) !*Node {
        const name = try self.consume(.identifier, "Expect function name");
        const func_node = try Node.init(self.allocator, .function_decl);
        func_node.data.function_decl.name = try self.allocator.dupe(u8, name.lexeme);

        _ = try self.consume(.left_paren, "Expect '(' after function name");

        // Parse parameters
        if (!self.check(.right_paren)) {
            while (true) {
                const param = try self.consume(.identifier, "Expect parameter name");
                const param_name = try self.allocator.dupe(u8, param.lexeme);
                try func_node.data.function_decl.params.append(param_name);

                if (!self.match(.comma)) {
                    break;
                }
            }
        }

        _ = try self.consume(.right_paren, "Expect ')' after parameters");
        _ = try self.consume(.left_brace, "Expect '{' before function body");

        // Parse function body
        func_node.data.function_decl.body = try self.blockStatement();

        return func_node;
    }

    fn importStatement(self: *Parser) !*Node {
        const module = try self.consume(.identifier, "Expect module name");
        const import_node = try Node.init(self.allocator, .import_stmt);
        import_node.data.import_stmt.module = try self.allocator.dupe(u8, module.lexeme);

        _ = try self.consume(.semicolon, "Expect ';' after import statement");

        return import_node;
    }

    fn varDeclaration(self: *Parser) !*Node {
        const name = try self.consume(.identifier, "Expect variable name");
        const var_node = try Node.init(self.allocator, .variable_decl);
        var_node.data.variable_decl.name = try self.allocator.dupe(u8, name.lexeme);

        // Check for initialization
        if (self.match(.equal)) {
            var_node.data.variable_decl.value = try self.expression();
        } else {
            var_node.data.variable_decl.value = null;
        }

        _ = try self.consume(.semicolon, "Expect ';' after variable declaration");

        return var_node;
    }

    fn statement(self: *Parser) !*Node {
        if (self.match(.left_brace)) {
            return try self.blockStatement();
        }
        if (self.match(.@"if")) {
            // TODO: Implement if statements
            return error.NotImplemented;
        }
        if (self.match(.print)) {
            return try self.printStatement();
        }
        if (self.match(.@"return")) {
            return try self.returnStatement();
        }

        return try self.expressionStatement();
    }

    fn blockStatement(self: *Parser) anyerror!*Node {
        const block = try Node.init(self.allocator, .block_stmt);

        while (!self.check(.right_brace) and !self.isAtEnd()) {
            const stmt = try self.declaration();
            try block.data.block_stmt.statements.append(stmt);
        }

        _ = try self.consume(.right_brace, "Expect '}' after block");

        return block;
    }

    fn printStatement(self: *Parser) !*Node {
        const expr = try self.expression();
        _ = try self.consume(.semicolon, "Expect ';' after value");

        // Create a call expression to "print"
        const print_ident = try Node.init(self.allocator, .identifier);
        print_ident.data.identifier.name = try self.allocator.dupe(u8, "print");

        const call_node = try Node.init(self.allocator, .call_expr);
        call_node.data.call_expr.callee = print_ident;
        try call_node.data.call_expr.args.append(expr);

        return call_node;
    }

    fn returnStatement(self: *Parser) !*Node {
        // TODO: Implement return statements
        _ = try self.expression();
        _ = try self.consume(.semicolon, "Expect ';' after return value");

        return error.NotImplemented;
    }

    fn expressionStatement(self: *Parser) !*Node {
        const expr = try self.expression();
        _ = try self.consume(.semicolon, "Expect ';' after expression");

        return expr;
    }

    fn expression(self: *Parser) !*Node {
        return try self.assignment();
    }

    fn assignment(self: *Parser) !*Node {
        const expr = try self.equality();

        if (self.match(.equal)) {
            const equals = self.previous();
            _ = equals;
            const value = try self.assignment();

            if (expr.type == .identifier) {
                const assign_node = try Node.init(self.allocator, .assignment);
                assign_node.data.assignment.name = try self.allocator.dupe(u8, expr.data.identifier.name);
                assign_node.data.assignment.value = value;
                return assign_node;
            }

            // Error: Invalid assignment target
            return error.InvalidAssignment;
        }

        return expr;
    }

    fn equality(self: *Parser) !*Node {
        var expr = try self.comparison();

        while (self.match(.bang_equal) or self.match(.equal_equal)) {
            const operator = self.previous();
            const right = try self.comparison();

            const binary_node = try Node.init(self.allocator, .binary_expr);
            binary_node.data.binary_expr.left = expr;
            binary_node.data.binary_expr.operator = operator;
            binary_node.data.binary_expr.right = right;

            expr = binary_node;
        }

        return expr;
    }

    fn comparison(self: *Parser) !*Node {
        var expr = try self.term();

        while (self.match(.greater) or self.match(.greater_equal) or
            self.match(.less) or self.match(.less_equal))
        {
            const operator = self.previous();
            const right = try self.term();

            const binary_node = try Node.init(self.allocator, .binary_expr);
            binary_node.data.binary_expr.left = expr;
            binary_node.data.binary_expr.operator = operator;
            binary_node.data.binary_expr.right = right;

            expr = binary_node;
        }

        return expr;
    }

    fn term(self: *Parser) !*Node {
        var expr = try self.factor();

        while (self.match(.minus) or self.match(.plus)) {
            const operator = self.previous();
            const right = try self.factor();

            const binary_node = try Node.init(self.allocator, .binary_expr);
            binary_node.data.binary_expr.left = expr;
            binary_node.data.binary_expr.operator = operator;
            binary_node.data.binary_expr.right = right;

            expr = binary_node;
        }

        return expr;
    }

    fn factor(self: *Parser) !*Node {
        var expr = try self.unary();

        while (self.match(.slash) or self.match(.star)) {
            const operator = self.previous();
            const right = try self.unary();

            const binary_node = try Node.init(self.allocator, .binary_expr);
            binary_node.data.binary_expr.left = expr;
            binary_node.data.binary_expr.operator = operator;
            binary_node.data.binary_expr.right = right;

            expr = binary_node;
        }

        return expr;
    }

    fn unary(self: *Parser) !*Node {
        if (self.match(.bang) or self.match(.minus)) {
            const operator = self.previous();
            const right = try self.unary();

            // For simplicity, we'll represent unary operations as binary with a null left
            // In a real implementation, we'd have a separate unary expression node
            const binary_node = try Node.init(self.allocator, .binary_expr);
            binary_node.data.binary_expr.left = try self.literalNull();
            binary_node.data.binary_expr.operator = operator;
            binary_node.data.binary_expr.right = right;

            return binary_node;
        }

        return try self.call();
    }

    fn call(self: *Parser) !*Node {
        var expr = try self.primary();

        while (true) {
            if (self.match(.left_paren)) {
                expr = try self.finishCall(expr);
            } else {
                break;
            }
        }

        return expr;
    }

    fn finishCall(self: *Parser, callee: *Node) anyerror!*Node {
        const call_node = try Node.init(self.allocator, .call_expr);
        call_node.data.call_expr.callee = callee;

        if (!self.check(.right_paren)) {
            while (true) {
                if (call_node.data.call_expr.args.items.len >= 255) {
                    // Error: Too many arguments
                    return error.TooManyArguments;
                }

                try call_node.data.call_expr.args.append(try self.expression());

                if (!self.match(.comma)) {
                    break;
                }
            }
        }

        _ = try self.consume(.right_paren, "Expect ')' after arguments");

        return call_node;
    }

    fn primary(self: *Parser) anyerror!*Node {
        if (self.match(.false)) {
            const literal = try Node.init(self.allocator, .literal);
            literal.data.literal.value = .{ .boolean = false };
            return literal;
        }
        if (self.match(.true)) {
            const literal = try Node.init(self.allocator, .literal);
            literal.data.literal.value = .{ .boolean = true };
            return literal;
        }
        if (self.match(.nil)) {
            return try self.literalNull();
        }
        if (self.match(.number)) {
            const literal = try Node.init(self.allocator, .literal);
            const token = self.previous();
            const value = try std.fmt.parseFloat(f64, token.lexeme);
            literal.data.literal.value = .{ .number = value };
            return literal;
        }
        if (self.match(.string)) {
            const literal = try Node.init(self.allocator, .literal);
            const token = self.previous();
            // Remove the quotes
            const str = token.lexeme[1 .. token.lexeme.len - 1];
            literal.data.literal.value = .{ .string = try self.allocator.dupe(u8, str) };
            return literal;
        }
        if (self.match(.identifier)) {
            const ident = try Node.init(self.allocator, .identifier);
            ident.data.identifier.name = try self.allocator.dupe(u8, self.previous().lexeme);
            return ident;
        }
        if (self.match(.left_paren)) {
            const expr = try self.expression();
            _ = try self.consume(.right_paren, "Expect ')' after expression");
            return expr;
        }

        // Error: Expect expression
        self.errorAtCurrent("Expect expression");
        return error.ParseError;
    }

    fn literalNull(self: *Parser) !*Node {
        const literal = try Node.init(self.allocator, .literal);
        literal.data.literal.value = .nil;
        return literal;
    }

    fn match(self: *Parser, token_type: TokenType) bool {
        if (self.check(token_type)) {
            _ = self.advance();
            return true;
        }
        return false;
    }

    fn check(self: *Parser, token_type: TokenType) bool {
        if (self.isAtEnd()) {
            return false;
        }
        return self.peek().type == token_type;
    }

    fn advance(self: *Parser) Token {
        if (!self.isAtEnd()) {
            self.current += 1;
        }
        return self.previous();
    }

    fn isAtEnd(self: *Parser) bool {
        return self.peek().type == .eof;
    }

    fn peek(self: *Parser) Token {
        return self.tokens.items[self.current];
    }

    fn previous(self: *Parser) Token {
        return self.tokens.items[self.current - 1];
    }

    fn consume(self: *Parser, token_type: TokenType, message: []const u8) !Token {
        if (self.check(token_type)) {
            return self.advance();
        }

        self.errorAtCurrent(message);
        return error.ParseError;
    }

    fn errorAt(self: *Parser, token: Token, message: []const u8) void {
        if (self.panic_mode) return;
        self.panic_mode = true;
        self.had_error = true;

        if (token.type == .eof) {
            std.debug.print("[line {d}] Error at end: {s}\n", .{ token.line, message });
        } else {
            std.debug.print("[line {d}] Error at '{s}': {s}\n", .{ token.line, token.lexeme, message });
        }
    }

    fn errorAtCurrent(self: *Parser, message: []const u8) void {
        self.errorAt(self.peek(), message);
    }

    fn reportError(self: *Parser, message: []const u8) void {
        self.errorAt(self.previous(), message);
    }

    fn synchronize(self: *Parser) void {
        self.panic_mode = false;

        while (!self.isAtEnd()) {
            if (self.previous().type == .semicolon) return;

            switch (self.peek().type) {
                .class, .func, .@"var", .@"for", .@"if", .@"while", .@"return", .import, .right_brace => return,
                else => {},
            }

            _ = self.advance();
        }
    }
};

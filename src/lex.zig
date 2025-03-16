const std = @import("std");

/// A Norg document lexer.
pub const Lexer = struct {
    /// The bytes that are being lexed.
    bytes: []const u8,

    /// The current position.
    pos: usize = 0,

    /// The last token that was eaten.
    previous_token: ?Token = null,

    /// Returns the remaining bytes in the lexer.
    pub inline fn remaining(lex: *Lexer) []const u8 {
        return lex.bytes[lex.pos..];
    }

    /// Returns `true` if the lexer has no more tokens.
    inline fn isEmpty(lex: *const Lexer) bool {
        return lex.pos >= lex.bytes.len;
    }

    /// Advances the lexer if the leading character(s) is a newline.
    /// Accepts CR, LF, and CRLF.
    fn eatNewline(lex: *Lexer) bool {
        // we consider CR, LF, and CRLF each as a single newline
        var ok = false;
        if (lex.bytes[lex.pos] == '\r') {
            lex.pos += 1;
            ok = true;
        }
        if (lex.bytes[lex.pos] == '\n') {
            lex.pos += 1;
            ok = true;
        }
        return ok;
    }

    /// Advances the lexer as long as the leading character is whitespace.
    fn eatWhitespace(lex: *Lexer) ?[]const u8 {
        const orig_pos = lex.pos;

        // advance while head char is whitespace
        while (lex.pos < lex.bytes.len and std.ascii.isWhitespace(lex.bytes[lex.pos])) : (lex.pos += 1) {}

        if (lex.pos == orig_pos) {
            return null;
        }
        return lex.bytes[orig_pos..lex.pos];
    }

    /// Advances the lexer as long as the leading character is alphanumeric.
    fn eatWord(lex: *Lexer) ?[]const u8 {
        const orig_pos = lex.pos;

        // advance while head char is alphanumeric
        while (lex.pos < lex.bytes.len and std.ascii.isAlphanumeric(lex.bytes[lex.pos])) : (lex.pos += 1) {}

        if (lex.pos == orig_pos) {
            return null;
        }
        return lex.bytes[orig_pos..lex.pos];
    }

    /// Advances the lexer by one character if the leading character is a symbol.
    fn eatSymbol(lex: *Lexer) ?Symbol {
        if (lex.isEmpty()) {
            return null;
        }

        const char = lex.bytes[lex.pos];
        if (Symbol.fromChar(char)) |sym| {
            lex.pos += 1;
            return sym;
        }

        if ((char < '0' and char > ' ') or (char < 'A' and char > '9') or (char < 'a' and char > 'Z')) {
            lex.pos += 1;
            return @enumFromInt(char);
        }

        return null;
    }

    fn eatTokenInner(lex: *Lexer) ?Token {
        if (lex.isEmpty()) {
            return null;
        }

        // note: this must be checked before `eatWhitespace`, because newline
        // characters are considered whitespace
        if (lex.eatNewline()) {
            return .newline;
        }

        if (lex.eatWhitespace()) |ws| {
            return Token{
                .whitespace = ws,
            };
        }

        if (lex.eatWord()) |word| {
            return Token{
                .word = word,
            };
        }

        if (lex.eatSymbol()) |sym| {
            return Token{
                .symbol = sym,
            };
        }

        return null;
    }

    /// Returns the next token.
    /// Returns `null` when there are no more tokens.
    pub fn eatToken(lex: *Lexer) ?Token {
        const token = lex.eatTokenInner();
        defer lex.previous_token = token;
        return token;
    }

    /// Advances the token.
    /// Convenient wrapper around `eatToken` when you've already peeked the next
    /// token and you don't want it.
    pub inline fn advance(lex: *Lexer) void {
        _ = lex.eatToken();
    }

    pub fn peekNthToken(lex: *const Lexer, n: usize) ?Token {
        var lex_copy = lex.*;

        // skip the first n-1 tokens
        for (0..n) |_| lex_copy.advance();

        // return the nth
        return lex_copy.eatToken();
    }

    pub fn peekToken(lex: *const Lexer) ?Token {
        return lex.peekNthToken(0);
    }
};

/// A token in the Norg document.
pub const Token = union(enum) {
    /// Some amount of whitespace.
    whitespace: []const u8,
    /// A newline.
    newline,
    /// A span of word characters - may not be a full word.
    word: []const u8,
    /// A punctuation character.
    symbol: Symbol,

    /// Returns `true` if this token is the given symbol.
    pub inline fn isSymbol(tok: Token, symbol: Symbol) bool {
        return tok == .symbol and tok.symbol == symbol;
    }

    /// Returns `true` if this token is a newline.
    pub inline fn isNewline(tok: Token) bool {
        return switch (tok) {
            .newline => true,
            else => false,
        };
    }
};

pub const Symbol = enum(u8) {
    /// `*` symbol.
    /// As a detached modifier, it denotes a header.
    /// As an attached modifier, it makes text bold.
    asterisk = '*',
    /// `-` symbol.
    /// As a detached modifier, it denotes an *unordered* list item.
    /// As an attached modifier, it makes text strikethrough.
    hyphen = '-',
    /// `~` symbol.
    /// As a detached modifier, it denotes an *ordered* list item.
    tilde = '~',
    /// `>` symbol.
    /// As a detached modifier, it denotes a block quote.
    angle_right = '>',
    /// `$` symbol.
    /// As a detached modifier, it denotes a definition.
    /// As an attached modifier, it makes an *inline math* span.
    dollar = '$',
    /// `^` symbol.
    /// As a detached modifier, it denotes a footnote.
    caret = '^',
    /// `:` symbol.
    /// As a detached modifier, it denotes a table cell.
    /// As an attached modifier, it makes text *superscript*.
    colon = ':',
    /// `%` symbol.
    /// TODO: this is used for attributes somehow?
    /// As an attached modifier, it's the *null modifier* - text inside is
    /// hidden.
    percent = '%',
    /// `|` symbol.
    /// This is called the "contextual delimiter."
    pipe = '|',
    /// `{` symbol.
    /// TODO: how is this used?
    curly_open = '{',
    /// `}` symbol.
    /// TODO: how is this used?
    curly_close = '}',
    /// `(` symbol.
    /// TODO: how is this used?
    paren_open = '(',
    /// `)` symbol.
    /// TODO: how is this used?
    paren_close = ')',
    /// `/` symbol.
    /// As an attached modifier, it makes text *italic*.
    forward_slash = '/',
    /// `_` symbol.
    /// As an attached modifier, it makes text *underlined*.
    underscore = '_',
    /// `!` symbol.
    /// As an attached modifier, it makes text *spoilered*.
    exclamation_point = '!',
    /// `,` symbol.
    /// As an attached modifier, it makes text *subscript*.
    comma = ',',
    /// \` symbol.
    /// As an attached modifier, it makes text *inline code*.
    backtick = '`',
    /// `&` symbol.
    /// TODO: attached modifier
    ampersand = '&',

    _,

    /// Returns the `Symbol` for the corresponding character if one exists,
    /// otherwise `null`.
    pub fn fromChar(c: u8) ?Symbol {
        return switch (c) {
            '*' => .asterisk,
            '-' => .hyphen,
            '~' => .tilde,
            '>' => .angle_right,
            '$' => .dollar,
            '^' => .caret,
            ':' => .colon,
            '%' => .percent,
            '|' => .pipe,
            '{' => .curly_open,
            '}' => .curly_close,
            '(' => .paren_open,
            ')' => .paren_close,
            '/' => .forward_slash,
            '_' => .underscore,
            '!' => .exclamation_point,
            ',' => .comma,
            '&' => .ampersand,
            else => null,
        };
    }

    /// Converts the `Symbol` to the corresponding character.
    pub inline fn toChar(sym: Symbol) u8 {
        return @intFromEnum(sym);
    }
};

// testing

fn testLexCommon(lex: *Lexer, actual_len: usize, expected_len: usize) !void {
    try std.testing.expectEqual(actual_len, expected_len);
    try std.testing.expectEqual(lex.pos, expected_len);
    try std.testing.expectEqualStrings(lex.remaining(), lex.bytes[expected_len..]);
}

// testing - whitespace
test "eatWhitespace eats whitespace" {
    var lex = Lexer{ .bytes = "    foo" };
    const ws = lex.eatWhitespace().?;
    try testLexCommon(&lex, ws.len, 4);
}

test "eatWhitespace does nothing when no whitespace" {
    var lex = Lexer{ .bytes = "foo" };
    const ws = lex.eatWhitespace();
    try std.testing.expect(ws == null);
}

test "eatWhitespace does nothing on empty string" {
    var lex = Lexer{ .bytes = "" };
    const ws = lex.eatWhitespace();
    try std.testing.expect(ws == null);
}

// testing - word
test "eatWord eats a word" {
    var lex = Lexer{ .bytes = "foo" };
    const word = lex.eatWord().?;
    try testLexCommon(&lex, word.len, 3);
}

test "eatWord eats only the first word" {
    var lex = Lexer{ .bytes = "foo bar" };
    const word = lex.eatWord().?;
    try testLexCommon(&lex, word.len, 3);
}

test "eatWord does not eat punctuation" {
    var lex = Lexer{ .bytes = "foo." };
    const word = lex.eatWord().?;
    try testLexCommon(&lex, word.len, 3);
}

test "eatWord returns null for empty string" {
    var lex = Lexer{ .bytes = "" };
    const word = lex.eatWord();
    try std.testing.expectEqual(null, word);
}

// testing - symbol
test "eatSymbol eats a symbol" {
    var lex = Lexer{ .bytes = "!" };
    const sym = lex.eatSymbol();
    try std.testing.expectEqual(.exclamation_point, sym);
    try std.testing.expect(sym != null);
}

test "eatSymbol eats only the first symbol" {
    var lex = Lexer{ .bytes = "!*" };
    const sym = lex.eatSymbol();
    try std.testing.expectEqual(.exclamation_point, sym);
    try testLexCommon(&lex, lex.pos, 1);
}

test "eatSymbol returns null for empty string" {
    var lex = Lexer{ .bytes = "" };
    const sym = lex.eatSymbol();
    try std.testing.expectEqual(null, sym);
}

test "eatSymbol returns null when no symbol" {
    var lex = Lexer{ .bytes = "foo!" };
    const sym = lex.eatSymbol();
    try std.testing.expectEqual(null, sym);
}

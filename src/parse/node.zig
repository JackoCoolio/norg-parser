const std = @import("std");
const Allocator = std.mem.Allocator;

const lex = @import("../lex.zig");
const Lexer = lex.Lexer;
const Token = lex.Token;
const Symbol = lex.Symbol;
const ast = @import("../ast.zig");
const Node = ast.Node;

const StyleStack = struct {
    len: usize,
    inner: [@typeInfo(Node.Style).@"enum".fields.len]?Node.Style,

    pub const empty = StyleStack{
        .len = 0,
        .inner = @splat(null),
    };

    pub fn findStyle(stack: *const StyleStack, style: Node.Style) ?usize {
        for (0..stack.inner.len) |i| {
            const this_style = stack.inner[i];
            if (this_style == style) {
                return i;
            }
        }
        return null;
    }

    /// Returns `true` if the stack contains the given `Style`.
    pub inline fn hasStyle(stack: *const StyleStack, style: Node.Style) bool {
        return stack.findStyle(style) != null;
    }

    pub fn push(stack: *StyleStack, style: Node.Style) void {
        if (stack.len >= stack.inner.len) {
            @panic("stack capacity too small");
        }

        stack.inner[stack.len] = style;
        // update length
        stack.len += 1;
    }

    pub fn popStyle(stack: *StyleStack, style: Node.Style) bool {
        if (stack.findStyle(style)) |start| {
            // TODO(perf): memset
            for (start..stack.len) |i| {
                stack.inner[i] = null;
            }
            // update length
            stack.len = start;
            return true;
        } else {
            return false;
        }
    }
};

test "StyleStack: popStyle works" {
    var stack = StyleStack.empty;

    // only .bold on stack
    stack.push(.bold);
    try std.testing.expectEqual(1, stack.len);
    try std.testing.expectEqual(true, stack.popStyle(.bold));
    try std.testing.expectEqual(0, stack.len);

    // .bold with .underline on top
    stack.push(.bold);
    stack.push(.underline);
    try std.testing.expectEqual(2, stack.len);
    try std.testing.expectEqual(true, stack.popStyle(.underline));
    try std.testing.expectEqual(1, stack.len); // only .underline popped

    stack.push(.italic);
    try std.testing.expectEqual(2, stack.len);
    try std.testing.expectEqual(true, stack.popStyle(.bold));
    try std.testing.expectEqual(0, stack.len); // *both* popped
}

fn isDoubled(lexer: *const Lexer, symbol: Symbol) bool {
    if (lexer.previous_token) |prev_token| {
        if (prev_token.isSymbol(symbol)) {
            return true;
        }
    }
    if (lexer.peekNthToken(1)) |next_token| {
        if (next_token.isSymbol(symbol)) {
            return true;
        }
    }
    return false;
}

fn parseInner(alloc: Allocator, lexer: *Lexer, style_stack: *StyleStack) !?Node {
    const start_pos = lexer.pos;
    var following_newline = false;
    var following_ws = true;

    while (lexer.peekToken()) |token| {
        if (lexer.previous_token) |prev_token| switch (prev_token) {
            .newline => {
                following_newline = true;
                following_ws = true;
            },
            .whitespace => {
                following_ws = true;
                following_newline = false;
            },
            .symbol => {
                following_newline = false;
                following_ws = true;
            },
            else => {
                following_ws = false;
                following_newline = false;
            },
        };

        switch (token) {
            .newline => {
                lexer.advance();
                if (following_newline) {
                    return Node.fromText(lexer.bytes[start_pos..lexer.pos]);
                }
            },
            .symbol => |sym| {
                if (Node.Style.fromSymbol(&sym)) |style| {
                    const next_tok_type: enum { word, ws, other } = if (lexer.peekNthToken(1)) |tok|
                        switch (tok) {
                            .word => |_| .word,
                            .whitespace => |_| .ws,
                            else => .other,
                        }
                    else
                        .ws;
                    const prev_tok_type: enum { word, ws, other } = if (lexer.previous_token) |tok|
                        switch (tok) {
                            .word => |_| .word,
                            .whitespace => |_| .ws,
                            else => .other,
                        }
                    else
                        .ws;

                    const is_doubled = isDoubled(lexer, sym);

                    // can close if:
                    // - previous character is NOT .whitespace
                    // - and next character is not .word
                    const can_close = (prev_tok_type != .ws) and (next_tok_type != .word) and !is_doubled;
                    // can open if:
                    // - previous character is NOT .word
                    // - and next character is NOT .whitespace
                    const can_open = (prev_tok_type != .word) and (next_tok_type != .ws) and !is_doubled;

                    if (can_close and style_stack.popStyle(style)) {
                        // we're closing this style

                        // note: we don't advance the lexer here, because
                        // that is handled above in the recursive case so
                        // that the closing symbol isn't included in the
                        // leaf node that we create below

                        return Node.fromText(lexer.bytes[start_pos..lexer.pos]);
                    }

                    if (can_open and !style_stack.hasStyle(style)) {
                        // opening a new style
                        const pre_text = lexer.bytes[start_pos..lexer.pos];
                        const pre_stack_len = style_stack.len;

                        style_stack.push(style);

                        // because !ws_is_next, we know that there is a valid Node up next
                        lexer.advance(); // skip the style opener
                        const inner_start_pos = lexer.pos;
                        var styled = try parseInner(alloc, lexer, style_stack);
                        var should_deinit_styled = true;
                        defer if (should_deinit_styled) if (styled) |s| s.deinit(alloc);
                        if (lexer.pos == inner_start_pos) {
                            // the closer immediately followed the opener - just
                            // continue, since this wasn't a valid opener
                            lexer.advance();
                            continue;
                        }

                        if (style_stack.popStyle(style)) {
                            // the style wasn't closed, because we either
                            // reached the end of the document or reached
                            // another style closer. either way, just return the
                            // leaf that we have up to here
                            try styled.?.mergeFront(alloc, lexer.bytes[start_pos..inner_start_pos]);
                            should_deinit_styled = false;
                            return styled;
                        }

                        if (style_stack.len < pre_stack_len) {
                            // we encountered a *different* style closer before
                            // we closed this one, so styled isn't any different
                            // from `pre_text`. that means we should just return
                            // unstyled text from where we started until the end
                            // of the "styled" text

                            // note: we don't eat the different closing token
                            // here, because the "successful" case below (where
                            // style_stack_len == pre_stack_len) handles eating
                            // the closing token.
                            //
                            // here, we're in the inner parse, parsing the text
                            // *within* the style markers. just because we found
                            // a style closer here doesn't mean we're allowed to
                            // eat it
                            return .{ .leaf = lexer.bytes[start_pos..lexer.pos] };
                        } else { // style_stack.len == pre_stack_len
                            // the style stack did not shrink any further,
                            // and thus our style was closed
                            var children: std.ArrayListUnmanaged(Node.Child) = try .initCapacity(alloc, 1);
                            if (Node.fromText(pre_text)) |node| {
                                children.addOneAssumeCapacity().* = .{
                                    .style = null,
                                    .node = blk: {
                                        const ptr = try alloc.create(Node);
                                        ptr.* = node;
                                        break :blk ptr;
                                    },
                                };
                            }

                            should_deinit_styled = false;
                            (try children.addOne(alloc)).* = .{
                                .style = style,
                                .node = blk: {
                                    const node = try alloc.create(Node);
                                    // see above - since the style was closed,
                                    // we know this isn't null
                                    node.* = styled.?;
                                    break :blk node;
                                },
                            };

                            // eat the closing token
                            lexer.advance();

                            // parse the rest!
                            if (try parseInner(alloc, lexer, style_stack)) |remaining| {
                                // concat those nodes onto the end of this one
                                switch (remaining) {
                                    .branch => |br| {
                                        try children.appendSlice(alloc, br.children);
                                        alloc.free(br.children);
                                    },
                                    .leaf => |_| {
                                        const new = try children.addOne(alloc);
                                        new.* = .{
                                            .style = null,
                                            .node = blk: {
                                                const node = try alloc.create(Node);
                                                node.* = remaining;
                                                break :blk node;
                                            },
                                        };
                                    },
                                }
                            }

                            return .{
                                .branch = .{
                                    .children = try children.toOwnedSlice(alloc),
                                },
                            };
                        }

                        continue;
                    }

                    // wedged between two words - no can do
                    lexer.advance();
                    // TODO(perf): call eatToken again (for the next word?)
                } else {
                    // just advance the lexer since we can't do anything
                    // with it
                    lexer.advance();
                }
            },
            .whitespace => {
                lexer.advance();
            },
            .word => {
                lexer.advance();
            },
        }
    }

    // if no more tokens, return what we have
    return Node.fromText(lexer.bytes[start_pos..lexer.pos]);
}

/// Parse a node.
pub fn parse(alloc: Allocator, lexer: *Lexer) !?Node {
    var style_stack: StyleStack = .empty;
    return try parseInner(alloc, lexer, &style_stack);
}

/// Executes a test case by parsing the given string of text and comparing it
/// to the expected node structure.
///
/// Use this function when an incorrect parse would still be textually
/// represented identically to the test string. For example, the string
/// "*foo**bar*" should parse as <b>foo**bar</b>, but an incorrect parse of
/// <b>foo</b><b>bar</b> would be represented the same.
///
/// This also executes `quickTestCase`.
fn testCase(text: []const u8, expected_structure: ?[]const u8) !void {
    // assert that the parsed structure deserializes to the same string
    try quickTestCase(text);

    var lexer = Lexer{ .bytes = text };
    const actual_node = try parse(std.testing.allocator, &lexer);
    if (actual_node) |node| {
        defer node.deinit(std.testing.allocator);

        var buf = std.ArrayList(u8).init(std.testing.allocator);
        defer buf.deinit();
        try node.toDebugString(buf.writer().any());

        if (expected_structure) |exp_struct| {
            try std.testing.expectEqualStrings(exp_struct, buf.items[0..buf.items.len]);
        } else {
            std.debug.panic("expected null\ngot {s}\n", .{buf.items[0..buf.items.len]});
        }
    } else {
        if (expected_structure) |exp_struct| {
            std.debug.panic("expected {s}\ngot null\n", .{exp_struct});
        }
    }
}

pub fn quickTestCase(text: []const u8) !void {
    var lexer = Lexer{ .bytes = text };
    const actual_node = try parse(std.testing.allocator, &lexer) orelse {
        if (text.len > 0) {
            @panic("parse returned null, but string was non-empty");
        }
        return;
    };
    defer actual_node.deinit(std.testing.allocator);

    var buf = std.ArrayList(u8).init(std.testing.allocator);
    defer buf.deinit();
    try actual_node.toString(buf.writer().any());

    if (std.testing.expectEqualStrings(text, buf.items[0..buf.items.len])) {} else |err| {
        buf.clearRetainingCapacity();
        try actual_node.toDebugString(buf.writer().any());
        std.debug.print("Parse structure:\n{s}\n", .{buf.items[0..buf.items.len]});
        return err;
    }
}

test "empty string" {
    try testCase("", null);
}

test "single plain-text word" {
    try testCase("foo",
        \\"foo"
    );
}

test "multiple plain-text word" {
    try testCase("foo bar",
        \\"foo bar"
    );
}

test "single styled word" {
    try testCase("*foo*",
        \\(bold "foo")
    );
}

test "sentence with styled word" {
    try testCase("look, *this* word is bold!",
        \\(
        \\  "look, "
        \\  bold "this"
        \\  " word is bold!"
        \\)
    );
}

test "sentence with two styled words" {
    try testCase("look, *this* word and /this/ one are bold!",
        \\(
        \\  "look, "
        \\  bold "this"
        \\  " word and "
        \\  italic "this"
        \\  " one are bold!"
        \\)
    );
}

test "sentence that ends with style" {
    try testCase("the last word is *bold*",
        \\(
        \\  "the last word is "
        \\  bold "bold"
        \\)
    );
}

test "sentence with nested styles" {
    try testCase("_a *b* c_",
        \\(
        \\  underline (
        \\    "a "
        \\    bold "b"
        \\    " c"
        \\  )
        \\)
    );
}

test "sentence where two styles end at the same point" {
    try testCase("_all underline, but some /italic/_",
        \\(
        \\  underline (
        \\    "all underline, but some "
        \\    italic "italic"
        \\  )
        \\)
    );
}

test "style closer between two symbols" {
    // should be <b>hello*</b>!
    try testCase("_hello*_!",
        \\(
        \\  underline "hello*"
        \\  "!"
        \\)
    );
}

test "unclosed style" {
    try testCase("_hello",
        \\"_hello"
    );
}

test "incorrectly overlapping styles" {
    try testCase("*a _b* c_",
        \\(
        \\  bold "a _b"
        \\  " c_"
        \\)
    );
}

test "empty style span" {
    // empty style span shouldn't be styled
    try testCase("**a",
        \\"**a"
    );
    try testCase("a ** a",
        \\"a ** a"
    );
}

test "nested empty style span" {
    try testCase("o *__* o",
        \\(
        \\  "o "
        \\  bold "__"
        \\  " o"
        \\)
    );
}

test "wedged symbol" {
    try testCase("a*b  c*",
        \\"a*b  c*"
    );
}

test "multiple same-style spans" {
    try testCase("*foo* *bar*",
        \\(
        \\  bold "foo"
        \\  " "
        \\  bold "bar"
        \\)
    );
}

test "mixed numbers and symbols in styled content" {
    try testCase("*hello123!*",
        \\(bold "hello123!")
    );
}

test "three-level nesting" {
    try testCase("_a *b /c/ d* e_",
        \\(
        \\  underline (
        \\    "a "
        \\    bold (
        \\      "b "
        \\      italic "c"
        \\      " d"
        \\    )
        \\    " e"
        \\  )
        \\)
    );
}

test "consecutive same-style markers with content" {
    // a style marker cannot close a style if it is followed by itself
    try testCase("*foo**bar*",
        \\(bold "foo**bar")
    );
}

test "double style marker is ignored" {
    try testCase("**bold**",
        \\"**bold**"
    );
}

test "valid style after unclosed style" {
    try testCase("a *b /c/",
        \\(
        \\  "a *b "
        \\  italic "c"
        \\)
    );
}

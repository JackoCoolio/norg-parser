const std = @import("std");
const Allocator = std.mem.Allocator;

const Lexer = @import("../lex.zig").Lexer;
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

                    // can close if:
                    // - previous character is NOT .whitespace
                    // - and next character is not .word
                    const can_close = (prev_tok_type != .ws) and (next_tok_type != .word);
                    // can open if:
                    // - previous character is NOT .word
                    // - and next character is NOT .whitespace
                    const can_open = (prev_tok_type != .word) and (next_tok_type != .ws);

                    if (!can_open and !can_close) {
                        // can't do anything with this :(
                        lexer.advance();
                        continue;
                    }

                    if (can_close and style_stack.popStyle(style)) {
                        // we're closing this style

                        // note: we don't advance the lexer here, because
                        // that is handled above in the recursive case so
                        // that the closing symbol isn't included in the
                        // leaf node that we create below

                        if (lexer.pos > start_pos) {
                            return .{ .leaf = lexer.bytes[start_pos..lexer.pos] };
                        } else {
                            return null;
                        }
                    }

                    if (can_open and !style_stack.hasStyle(style)) { // and !ws_is_next
                        // opening a new style
                        var pre_text = lexer.bytes[start_pos..lexer.pos];
                        const pre_stack_len = style_stack.len;

                        style_stack.push(style);

                        // because !ws_is_next, we know that there is a valid Node up next
                        lexer.advance(); // skip the style opener
                        const styled = try parseInner(alloc, lexer, style_stack);

                        if (style_stack.hasStyle(style)) {
                            // the style wasn't closed, because we either reached
                            // the end of the document or reached another style
                            // closer. either way, just return the leaf that we
                            // have up to here
                            return Node.fromText(lexer.bytes[start_pos..lexer.pos]);
                        }

                        if (style_stack.len < pre_stack_len) {
                            // we encountered a *different* style closer
                            // before we closed this one - advance text
                            // and return text node
                            const text = lexer.bytes[start_pos..lexer.pos];
                            // eat the closing token
                            lexer.advance();
                            std.log.err("error A", .{});
                            return .{ .leaf = text };
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

                        // check if the style wasn't closed - if not, we should just
                        // consider everything from pre to now unstyled
                        if (!style_stack.hasStyle(style)) {
                            pre_text = lexer.bytes[start_pos..lexer.pos];
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

fn testNodeParse(text: []const u8, expected_node: ?Node) !void {
    var lexer = Lexer{ .bytes = text };
    const actual_node = try parse(std.testing.allocator, &lexer);
    if (actual_node) |node| {
        defer node.deinit(std.testing.allocator);
        try std.testing.expectEqualDeep(expected_node, actual_node);
    } else {
        try std.testing.expectEqual(expected_node, actual_node);
    }
}

test "Node.parse returns null when empty" {
    try testNodeParse("", null);
}

test "Node.parse parses plain single word" {
    try testNodeParse("foo", Node{
        .leaf = "foo",
    });
}

test "Node.parse parses multiple plain words" {
    try testNodeParse("foo bar", Node{
        .leaf = "foo bar",
    });
}

test "Node.parse parses a singularly, fully-styled word" {
    try testNodeParse("*foo*", Node{
        .branch = .{
            .children = &[_]Node.Child{
                .{
                    .style = .bold,
                    .node = &.{
                        .leaf = "foo",
                    },
                },
            },
        },
    });
}

test "Node.parse parses a sentence with a single styled word" {
    try testNodeParse("look, *this* word is bold!", Node{
        .branch = .{
            .children = &[_]Node.Child{
                .{
                    .style = null,
                    .node = &.{ .leaf = "look, " },
                },
                .{
                    .style = .bold,
                    .node = &.{
                        .leaf = "this",
                    },
                },
                .{
                    .style = null,
                    .node = &.{
                        .leaf = " word is bold!",
                    },
                },
            },
        },
    });
}

test "Node.parse parses a sentence with two styled words" {
    try testNodeParse("look, *this* word and /this/ one are bold!", Node{
        .branch = .{
            .children = &[_]Node.Child{
                .{
                    .style = null,
                    .node = &.{ .leaf = "look, " },
                },
                .{
                    .style = .bold,
                    .node = &.{
                        .leaf = "this",
                    },
                },
                .{
                    .style = null,
                    .node = &.{
                        .leaf = " word and ",
                    },
                },
                .{
                    .style = .italic,
                    .node = &.{
                        .leaf = "this",
                    },
                },
                .{
                    .style = null,
                    .node = &.{
                        .leaf = " one are bold!",
                    },
                },
            },
        },
    });
}

test "Node.parse parses a sentence that ends with a style" {
    try testNodeParse("the last word is *bold*", .{
        .branch = .{
            .children = &[_]Node.Child{
                .{
                    .style = null,
                    .node = &.{ .leaf = "the last word is " },
                },
                .{
                    .style = .bold,
                    .node = &.{ .leaf = "bold" },
                },
            },
        },
    });
}

test "sentence with nested styles" {
    try testNodeParse("_a *b* c_", .{
        .branch = .{
            .children = &[_]Node.Child{
                .{
                    .style = .underline,
                    .node = &.{
                        .branch = .{
                            .children = &[_]Node.Child{
                                .{
                                    .style = null,
                                    .node = &.{ .leaf = "a " },
                                },
                                .{
                                    .style = .bold,
                                    .node = &.{ .leaf = "b" },
                                },
                                .{
                                    .style = null,
                                    .node = &.{ .leaf = " c" },
                                },
                            },
                        },
                    },
                },
            },
        },
    });
}

test "Node.parse parses a sentence with nested styles" {
    try testNodeParse("_all underline, but some /italic/_", .{
        .branch = .{
            .children = &[_]Node.Child{
                .{
                    .style = .underline,
                    .node = &.{
                        .branch = .{
                            .children = &[_]Node.Child{
                                .{
                                    .style = null,
                                    .node = &.{ .leaf = "all underline, but some " },
                                },
                                .{
                                    .style = .italic,
                                    .node = &.{ .leaf = "italic" },
                                },
                            },
                        },
                    },
                },
            },
        },
    });
}

test "weird case" {
    // should be <b>hello*</b>!
    try testNodeParse("_hello*_!", .{
        .branch = .{
            .children = &[_]Node.Child{
                .{
                    .style = .underline,
                    .node = &.{ .leaf = "hello*" },
                },
                .{
                    .style = null,
                    .node = &.{ .leaf = "!" },
                },
            },
        },
    });
}

test "unclosed style" {
    try testNodeParse("_hello", .{
        .leaf = "_hello",
    });
}

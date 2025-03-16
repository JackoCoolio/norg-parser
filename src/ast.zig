const std = @import("std");
pub const Allocator = std.mem.Allocator;
const lex = @import("./lex.zig");
const Lexer = lex.Lexer;

pub const Document = struct {
    elements: []Block,
};

pub const Block = union(enum) {
    heading: Heading,
    paragraph: Node,
    list: List,
    quote: Quote,
    definition: Definition,
    footnote: Footnote,
    table: Table,
    code_block: CodeBlock,
    comment: Comment,
    tag: Tag,
};

pub const Heading = struct {
    level: u8,
    title: void, // todo
};

pub const List = struct {
    ordered: bool,
    items: []const ListItem,
};

pub const ListItem = struct {
    level: u8,
    content: []const Block,
};

pub const Quote = struct {
    level: u8,
    content: []const Block,
};

pub const Definition = struct {
    term: Node,
    content: []const Block,
    ranged: bool,
};

pub const Footnote = struct {
    label: []const u8,
    content: []const Block,
    ranged: bool,
};

pub const Table = struct {
    rows: []const TableRow,
};

pub const TableRow = struct {
    cells: []const TableCell,
};

pub const TableCell = struct {
    identifier: []const u8,
    content: []const Block,
};

pub const CodeBlock = struct {
    language: []const u8,
    content: []const u8,
};

pub const Comment = struct {
    content: []const u8,
};

pub const Tag = struct {
    name: []const u8,
    attributes: []const TagAttribute,
    content: ?[]const Block,
};

pub const TagAttribute = struct {
    key: []const u8,
    value: ?[]const u8,
};

pub const Node = union(enum) {
    branch: struct {
        children: []Child,
    },
    leaf: []const u8,

    pub fn fromText(text: []const u8) ?Node {
        if (text.len == 0) {
            return null;
        } else {
            return .{ .leaf = text };
        }
    }

    pub fn deinit(node: *const Node, alloc: Allocator) void {
        switch (node.*) {
            .branch => |br| {
                // deinit children
                for (br.children) |child| child.deinit(alloc);
                // deinit array
                alloc.free(br.children);
            },
            // leaf text is not owned by `Node`, so we do
            // not need to free it here
            .leaf => {},
        }
    }

    pub fn mergeFront(node: *Node, alloc: Allocator, text: []const u8) !void {
        if (text.len == 0) {
            // the node will end up being null, so ignore it
            return;
        }

        switch (node.*) {
            .leaf => |this_text| {
                // assert that they're mergeable
                std.debug.assert(text.ptr + text.len == this_text.ptr);

                node.leaf.ptr -= text.len;
                node.leaf.len += text.len;
            },
            .branch => |branch| {
                // branch should never be empty
                std.debug.assert(branch.children.len > 0);

                const first_child = branch.children[0];
                if (first_child.style == null) {
                    // there isn't a style boundary, so we can merge onto the
                    // first child
                    try first_child.node.mergeFront(alloc, text);
                } else {
                    var new_children = try alloc.alloc(Child, branch.children.len + 1);

                    // copy over the old, offset by one
                    @memcpy(new_children[1..], branch.children);

                    // create the new child
                    new_children[0].style = null;
                    new_children[0].node = try alloc.create(Node);
                    new_children[0].node.* = .{ .leaf = text };

                    // replace the old children with the new
                    alloc.free(branch.children);

                    // `node.` here because `branch` is immutable
                    node.branch.children = new_children;
                }
            },
        }
    }

    pub const Child = struct {
        node: *Node,
        style: ?Style = null,

        pub fn deinit(child: *const Child, alloc: Allocator) void {
            child.node.deinit(alloc);
            alloc.destroy(child.node);
        }
    };

    pub const Style = enum {
        bold,
        italic,
        underline,
        strikethrough,
        spoiler,
        superscript,
        subscript,
        null,
        inline_code,
        math,
        variable,
        // TODO: links?

        pub fn fromSymbol(sym: *const lex.Symbol) ?Style {
            return switch (sym.*) {
                .asterisk => .bold,
                .forward_slash => .italic,
                .underscore => .underline,
                .hyphen => .strikethrough,
                .exclamation_point => .spoiler,
                .caret => .superscript,
                .comma => .subscript,
                .backtick => .inline_code,
                .percent => .null,
                .dollar => .math,
                .ampersand => .variable,
                else => null,
            };
        }

        pub fn toSymbol(style: Style) lex.Symbol {
            return switch (style) {
                .bold => .asterisk,
                .italic => .forward_slash,
                .underline => .underscore,
                .strikethrough => .hyphen,
                .spoiler => .exclamation_point,
                .superscript => .caret,
                .subscript => .comma,
                .null => .percent,
                .inline_code => .backtick,
                .math => .dollar,
                .variable => .ampersand,
            };
        }
    };

    fn toDebugStringInner(node: *const Node, writer: std.io.AnyWriter, depth: u16) !void {
        switch (node.*) {
            .leaf => |text| {
                // text in quotes
                try writer.writeByte('"');
                _ = try writer.write(text);
                try writer.writeByte('"');
            },
            .branch => |branch| {
                try writer.writeByte('(');
                if (branch.children.len == 1 and branch.children[0].node.* == .leaf) {
                    if (branch.children[0].style) |style| {
                        _ = try writer.write(@tagName(style));
                        try writer.writeByte(' ');
                    }
                    try branch.children[0].node.toDebugStringInner(writer, 0);
                } else {
                    try writer.writeByte('\n');
                    for (branch.children) |child| {
                        try writer.writeByteNTimes(' ', (depth + 1) * 2);
                        if (child.style) |style| {
                            _ = try writer.write(@tagName(style));
                            try writer.writeByte(' ');
                        }
                        try child.node.toDebugStringInner(writer, depth + 1);
                        try writer.writeByte('\n');
                    }
                    try writer.writeByteNTimes(' ', depth * 2);
                }
                try writer.writeByte(')');
            },
        }
    }

    pub fn toDebugString(node: *const Node, writer: std.io.AnyWriter) !void {
        try node.toDebugStringInner(writer, 0);
    }

    pub fn toString(node: *const Node, writer: std.io.AnyWriter) !void {
        switch (node.*) {
            .leaf => |text| _ = try writer.write(text),
            .branch => |branch| {
                for (branch.children) |child| {
                    if (child.style) |style| try writer.writeByte(style.toSymbol().toChar());
                    try child.node.toString(writer);
                    if (child.style) |style| try writer.writeByte(style.toSymbol().toChar());
                }
            },
        }
    }
};

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
        children: []const Child,
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

    pub const Child = struct {
        node: *const Node,
        style: ?Style,

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
    };
};

const std = @import("std");
const indexOfPos = std.mem.indexOfPos;
const ArrayList = std.ArrayList;

const in = std.io.getStdIn().reader();
const out = std.io.getStdOut().writer();

const Board = struct {
    width: usize,
    height: usize,
    buf: []u8,
    allocator: std.mem.Allocator,

    fn init(allocator: std.mem.Allocator, width: usize, height: usize) !Board {
        const buf = try allocator.alloc(u8, width * height);
        @memset(buf, ' ');
        return Board {
            .width = width,
            .height = height,
            .buf = buf,
            .allocator = allocator
        };
    }

    fn deinit(self: Board) void {
        self.allocator.free(self.buf);
    }

    fn row(self: Board, i: usize) []u8 {
        const pos = i * self.width;
        return self.buf[pos..pos+self.width];
    }

    fn get(self: Board, x: usize, y: usize) u8 {
        return self.buf[x + y * self.width];
    }

    fn set(self: Board, x: usize, y: usize, v: u8) void {
        self.buf[x + y * self.width] = v;
    }
};

pub fn main() !void {

    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();

    //try part1(allocator);
    try part2(allocator);
}

fn part1(allocator: std.mem.Allocator) !void {
    const board = try parseInput(allocator, in);
    defer board.deinit();
    const transposed = try transpose(allocator, board);
    defer transposed.deinit();
    const left = try rotateLeft(allocator, board);
    defer left.deinit();
    const right = try rotateRight(allocator, board);
    defer right.deinit();

    const count = countInBoard(board, "XMAS")
        + countInBoard(board, "SAMX")
        + countInBoard(transposed, "XMAS")
        + countInBoard(transposed, "SAMX")
        + countInBoard(left, "XMAS")
        + countInBoard(left, "SAMX")
        + countInBoard(right, "XMAS")
        + countInBoard(right, "SAMX");

    try out.print("Count:{}\n", .{count});
}

fn part2(allocator: std.mem.Allocator) !void {
    const board = try parseInput(allocator, in);
    defer board.deinit();

    var count: u32 = 0;
    for (1..board.height-1) |y| {
        for (1..board.width-1) |x| {
            if (board.get(x, y) == 'A') {
                const nw = board.get(x-1, y-1);
                const ne = board.get(x+1, y-1);
                const sw = board.get(x-1, y+1);
                const se = board.get(x+1, y+1);

                if (isMS(nw, se) and isMS(ne, sw)) {
                    count += 1;
                }
            }
        }
    }
    try out.print("Count:{}\n", .{count});
}

fn isMS(p1: u8, p2: u8) bool {
    return (p1 == 'M' and p2 == 'S') or (p1 == 'S' and p2 == 'M');
}

fn countInBoard(board: Board, needle: []const u8) u32 {
    var count: u32 = 0;
    for (0..board.height) |i| {
        count += countInString(board.row(i), needle);
    }
    return count;
}

fn countInString(haystack: []const u8, needle: []const u8) u32 {
    var found: u32 = 0;
    var pos: usize = 0;
    const len = needle.len;
    while (indexOfPos(u8, haystack, pos, needle)) |i| {
        found += 1;
        pos = i + len;
    }
    return found;
}

fn parseInput(allocator: std.mem.Allocator, reader: anytype) !Board {
    var buf: [1000]u8 = undefined;
    var lines = ArrayList([]const u8).init(allocator);
    defer lines.deinit();
    while (try reader.readUntilDelimiterOrEof(&buf, '\n')) |line| {
        try lines.append(try allocator.dupe(u8, line));
    }
    defer {
        for (lines.items) |line| {
            allocator.free(line);
        }
    }

    const width = lines.items[0].len;
    const height = lines.items.len;
    var board = try Board.init(allocator, width, height);

    var i: usize = 0;
    for (lines.items) |line| {
        @memcpy(board.row(i), line[0..width]);
        i += 1;
    }
    return board;
}

fn transpose(allocator: std.mem.Allocator, board: Board) !Board {
    var transposed = try Board.init(allocator, board.height, board.width);

    for (0..board.width) |i| {
        for (0..board.height) |j| {
            transposed.set(j, i, board.get(i, j));
        }
    }
    return transposed;
}

fn rotateLeft(allocator: std.mem.Allocator, board: Board) !Board {
    var transposed = try Board.init(allocator, @min(board.width, board.height), board.width + board.height - 1);

    for (0..board.width) |i| {
        const x = board.width - i - 1;
        for (0..@min(i + 1, board.height)) |j| {
            transposed.set(j, i, board.get(x + j, j));
        }
    }
    for (1..board.height) |y| {
        const j = y - 1;
        for (0..@min(board.width, board.height - j - 1)) |x| {
            transposed.set(x, j + board.width, board.get(x, y + x));
        }
    }
    return transposed;
}

fn rotateRight(allocator: std.mem.Allocator, board: Board) !Board {
    var transposed = try Board.init(allocator, @min(board.width, board.height), board.width + board.height - 1);

    for (0..board.width) |i| {
        const x = i;
        for (0..@min(i + 1, board.height)) |j| {
            transposed.set(j, i, board.get(x - j, j));
        }
    }
    for (1..board.height) |y| {
        const j = y - 1;
        for (0..@min(board.width, board.height - j - 1)) |i| {
            const x = board.width - i - 1;
            transposed.set(i, j + board.width, board.get(x, y + i));
        }
    }
    return transposed;
}

const expectEqual = std.testing.expectEqual;
const expectEqualStrings = std.testing.expectEqualStrings;

test "board has default value" {
    const board = try Board.init(std.testing.allocator, 3, 2);
    defer board.deinit();
    try expectEqual(3, board.width);
    try expectEqual(2, board.height);
    try expectEqualStrings("   ", board.row(0));
    try expectEqualStrings("   ", board.row(1));
}

test "countInString" {
    try expectEqual(0, countInString("XMA", "XMAS"));
    try expectEqual(1, countInString("XMAS", "XMAS"));
    try expectEqual(2, countInString("XMAS...XMAS", "XMAS"));
    try expectEqual(2, countInString("...XMAS...XMAS...", "XMAS"));
    try expectEqual(2, countInString("...XMAXMAS...XMAS...", "XMAS"));
}

test "parseInput" {
    const input = "XMAS\nXMAS";
    var fbs = std.io.fixedBufferStream(input);
    const board = try parseInput(std.testing.allocator, fbs.reader());
    defer board.deinit();
    try expectEqual(4, board.width);
    try expectEqual(2, board.height);
    try expectEqualStrings("XMAS", board.row(0));
    try expectEqualStrings("XMAS", board.row(1));
}

test "parseInput2" {
    const input = "ABCD\nabcd\n1234";
    var fbs = std.io.fixedBufferStream(input);
    const board = try parseInput(std.testing.allocator, fbs.reader());
    defer board.deinit();
    try expectEqual(4, board.width);
    try expectEqual(3, board.height);
    try expectEqualStrings("ABCD", board.row(0));
    try expectEqualStrings("abcd", board.row(1));
    try expectEqualStrings("1234", board.row(2));
}

test "transpose" {
    var fbs = std.io.fixedBufferStream("XMAS\nXMAS");
    const board = try parseInput(std.testing.allocator, fbs.reader());
    defer board.deinit();

    const transposed = try transpose(std.testing.allocator, board);
    defer transposed.deinit();

    try expectEqual(2, transposed.width);
    try expectEqual(4, transposed.height);
    try expectEqualStrings("XX", transposed.row(0));
    try expectEqualStrings("MM", transposed.row(1));
    try expectEqualStrings("AA", transposed.row(2));
    try expectEqualStrings("SS", transposed.row(3));
}

test "rotateLeftLong" {
    var fbs = std.io.fixedBufferStream("ABC\nabc\nDEF\ndef");
    const board = try parseInput(std.testing.allocator, fbs.reader());
    defer board.deinit();

    const transposed = try rotateLeft(std.testing.allocator, board);
    defer transposed.deinit();

    try expectEqual(3, transposed.width);
    try expectEqual(6, transposed.height);
    try expectEqualStrings("C  ", transposed.row(0));
    try expectEqualStrings("Bc ", transposed.row(1));
    try expectEqualStrings("AbF", transposed.row(2));
    try expectEqualStrings("aEf", transposed.row(3));
    try expectEqualStrings("De ", transposed.row(4));
    try expectEqualStrings("d  ", transposed.row(5));
}

test "rotateLeftWide" {
    var fbs = std.io.fixedBufferStream("ABCD\nabcd\nEFGH");
    const board = try parseInput(std.testing.allocator, fbs.reader());
    defer board.deinit();
    try expectEqual(4, board.width);
    try expectEqual(3, board.height);

    const transposed = try rotateLeft(std.testing.allocator, board);
    defer transposed.deinit();

    try expectEqual(3, transposed.width);
    try expectEqual(6, transposed.height);
    try expectEqualStrings("D  ", transposed.row(0));
    try expectEqualStrings("Cd ", transposed.row(1));
    try expectEqualStrings("BcH", transposed.row(2));
    try expectEqualStrings("AbG", transposed.row(3));
    try expectEqualStrings("aF ", transposed.row(4));
    try expectEqualStrings("E  ", transposed.row(5));
}

test "rotateRightLong" {
    var fbs = std.io.fixedBufferStream("ABC\nabc\nDEF\ndef");
    const board = try parseInput(std.testing.allocator, fbs.reader());
    defer board.deinit();

    const transposed = try rotateRight(std.testing.allocator, board);
    defer transposed.deinit();

    try expectEqual(3, transposed.width);
    try expectEqual(6, transposed.height);
    try expectEqualStrings("A  ", transposed.row(0));
    try expectEqualStrings("Ba ", transposed.row(1));
    try expectEqualStrings("CbD", transposed.row(2));
    try expectEqualStrings("cEd", transposed.row(3));
    try expectEqualStrings("Fe ", transposed.row(4));
    try expectEqualStrings("f  ", transposed.row(5));
}

test "rotateRightWide" {
    var fbs = std.io.fixedBufferStream("ABCD\nabcd\nEFGH");
    const board = try parseInput(std.testing.allocator, fbs.reader());
    defer board.deinit();
    try expectEqual(4, board.width);
    try expectEqual(3, board.height);

    const transposed = try rotateRight(std.testing.allocator, board);
    defer transposed.deinit();

    try expectEqual(3, transposed.width);
    try expectEqual(6, transposed.height);
    try expectEqualStrings("A  ", transposed.row(0));
    try expectEqualStrings("Ba ", transposed.row(1));
    try expectEqualStrings("CbE", transposed.row(2));
    try expectEqualStrings("DcF", transposed.row(3));
    try expectEqualStrings("dG ", transposed.row(4));
    try expectEqualStrings("H  ", transposed.row(5));
}

test "countInBoard" {
    const input = "XMAS\nxMAS";
    var fbs = std.io.fixedBufferStream(input);
    const board = try parseInput(std.testing.allocator, fbs.reader());
    defer board.deinit();
    try expectEqual(1, countInBoard(board, "XMAS"));
}

const std = @import("std");
const ArrayList = std.ArrayList;

const in = std.io.getStdIn().reader();
const out = std.io.getStdOut().writer();

const Board = struct {
    buf: []u8,
    dir: []i8,
    width: usize,
    allocator: std.mem.Allocator,

    fn init(allocator: std.mem.Allocator, width: usize, buf: []u8, dir: []i8) !Board {
        return Board {
            .buf = try allocator.dupe(u8, buf),
            .dir = try allocator.dupe(i8, dir),
            .width = width,
            .allocator = allocator
        };
    }

    fn deinit(self: Board) void {
        self.allocator.free(self.buf);
        self.allocator.free(self.dir);
    }

    fn replace(self: *Board, buf: []u8) !void {
        const new_buf = try self.allocator.dupe(u8, buf);
        self.allocator.free(self.buf);
        self.buf = new_buf;
    }
};

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();

    var board = try parseInput(allocator, in);
    defer board.deinit();

    // try part1(allocator, &board);
    try part2(allocator, &board);
}

fn part1(_: std.mem.Allocator, board: *Board) !void {
    var pos: i16 = @intCast(std.mem.indexOfScalar(u8, board.buf, '@').?);
    for (board.dir) |dir| {
        if (move(board, pos, dir)) {
            pos += dir;
        }
    }

    try out.print("Score:{}\n", .{score(board, 'O')});
}

fn part2(allocator: std.mem.Allocator, board: *Board) !void {
    try scaleUp(allocator, board);

    var pos: i16 = @intCast(std.mem.indexOfScalar(u8, board.buf, '@').?);

    for (board.dir) |dir| {
        if (move_2(board, pos, dir)) {
            pos += dir;
        }
    }

    try out.print("Score:{}\n", .{score(board, '[')});
}

fn score(board: *Board, needle: u8) u64 {
    var pos: usize = 0;
    const width: usize = @intCast(board.width);

    var sum: u64 = 0;
    while (std.mem.indexOfScalarPos(u8, board.buf, pos, needle)) |new_pos| {
        sum += @mod(new_pos, width) + 100 * @divTrunc(new_pos, width);
        pos = new_pos + 1;
    }
    return sum;
}

fn move(board: *Board, pos: i16, dir: i8) bool {
    const old_pos: usize = @intCast(pos);
    const new_pos: usize = @intCast(pos + dir);
    const old_piece = board.buf[old_pos];
    const new_piece = board.buf[new_pos];

    const moved = switch (new_piece) {
        '#' => false,
        '.' => true,
        'O' => move(board, pos + dir, dir),
        else => unreachable
    };

    if (moved) {
        board.buf[new_pos] = old_piece;
        board.buf[old_pos] = '.';
    }
    return moved;
}

fn move_2(board: *Board, pos: i16, dir: i8) bool {
    if (can_move(board, pos, dir)) {
        do_move(board, pos, dir);
        return true;
    }
    return false;
}

fn can_move(board: *Board, pos: i16, dir: i8) bool {
    const new_pos: usize = @intCast(pos + dir);
    const new_piece = board.buf[new_pos];
    const is_vertical = @abs(dir) > 1;

    return switch (new_piece) {
        '#' => false,
        '.' => true,
        '[' => can_move(board, pos + dir, dir) and (!is_vertical or can_move(board, pos + dir + 1, dir)),
        ']' => can_move(board, pos + dir, dir) and (!is_vertical or can_move(board, pos + dir - 1, dir)),
        else => unreachable
    };
}

fn do_move(board: *Board, pos: i16, dir: i8) void {
    const old_pos: usize = @intCast(pos);
    const new_pos: usize = @intCast(pos + dir);
    const old_piece = board.buf[old_pos];
    const new_piece = board.buf[new_pos];
    const is_vertical = @abs(dir) > 1;

    if (new_piece == '[') {
        do_move(board, pos + dir, dir);
        if (is_vertical) {
            do_move(board, pos + dir + 1, dir);
        }
    } else if (new_piece == ']') {
        do_move(board, pos + dir, dir);
        if (is_vertical) {
            do_move(board, pos + dir - 1, dir);
        }
    }

    board.buf[new_pos] = old_piece;
    board.buf[old_pos] = '.';
}

fn parseInput(allocator: std.mem.Allocator, reader: anytype) !Board {
    var buf: [4000]u8 = undefined;
    var board = ArrayList(u8).init(allocator);
    defer board.deinit();
    var dir = ArrayList(i8).init(allocator);
    defer dir.deinit();
    var width: i8 = 0;
    while (try reader.readUntilDelimiterOrEof(&buf, '\n')) |line| {
        if (line.len > 0) {
            if (width == 0) {
                width = @intCast(line.len);
            }
            if (std.mem.indexOfScalar(u8, line, '#')) |_| {
                try board.appendSlice(line);
            } else {
                for (line) |ch| {
                    switch (ch) {
                        '^' => try dir.append(-width),
                        'v' => try dir.append(width),
                        '<' => try dir.append(-1),
                        '>' => try dir.append(1),
                        else => {},
                    }
                }
            }
        }
    }

    return try Board.init(allocator, @intCast(width), board.items, dir.items);
}

fn scaleUp(allocator: std.mem.Allocator, board: *Board) !void {
    var new_buf = try allocator.alloc(u8, board.buf.len * 2);
    defer allocator.free(new_buf);
    var new_pos: usize = 0;
    for (board.buf) |ch| {
         const new_str = switch (ch) {
            '#' => "##",
            'O' => "[]",
            '.' => "..",
            '@' => "@.",
            else => unreachable
        };
        @memcpy(new_buf[new_pos..new_pos+2], new_str);
        new_pos += 2;
    }
    for (0..board.dir.len) |i| {
        if (@abs(board.dir[i]) > 1) {
            board.dir[i] *= 2;
        }
    }
    board.width *= 2;
    try board.replace(new_buf);
}

const expectEqual = std.testing.expectEqual;
const expectEqualSlices = std.testing.expectEqualSlices;

test "parseInput" {
    const input = "#####\n#@.O#\n#O.O#\n#####\n\n^>><^v\n^^";
    var fbs = std.io.fixedBufferStream(input);
    const board = try parseInput(std.testing.allocator, fbs.reader());
    defer board.deinit();

    try expectEqual(5, board.width);
    try expectEqualSlices(u8, "#####", board.buf[0..5]);
    try expectEqualSlices(u8, "#@.O#", board.buf[5..10]);
    try expectEqualSlices(u8, "#O.O#", board.buf[10..15]);
    try expectEqualSlices(u8, "#####", board.buf[15..20]);

    try expectEqualSlices(i8, &[_]i8 { -5, 1, 1, -1, -5, 5, -5, -5 }, board.dir);
}

test "move" {
    const input = "#####\n#@O.#\n#O.O#\n#####\n\n>";
    var fbs = std.io.fixedBufferStream(input);
    var board = try parseInput(std.testing.allocator, fbs.reader());
    defer board.deinit();

    try expectEqualSlices(u8, "#####", board.buf[0..5]);
    try expectEqualSlices(u8, "#@O.#", board.buf[5..10]);
    try expectEqualSlices(u8, "#O.O#", board.buf[10..15]);
    try expectEqualSlices(u8, "#####", board.buf[15..20]);

    _ = move(&board, 6, 1);

    try expectEqualSlices(u8, "#####", board.buf[0..5]);
    try expectEqualSlices(u8, "#.@O#", board.buf[5..10]);
    try expectEqualSlices(u8, "#O.O#", board.buf[10..15]);
    try expectEqualSlices(u8, "#####", board.buf[15..20]);
}

test "score" {
    const input = "#######\n#...O..\n#......\n\n>";
    var fbs = std.io.fixedBufferStream(input);
    var board = try parseInput(std.testing.allocator, fbs.reader());
    defer board.deinit();

    try expectEqual(104, score(&board, 'O'));
}

test "scaleUp" {
    const allocator = std.testing.allocator;
    const input = "#####\n#@O.#\n#O.O#\n#####\n\n>v^<";
    var fbs = std.io.fixedBufferStream(input);
    var board = try parseInput(allocator, fbs.reader());
    defer board.deinit();

    try scaleUp(allocator, &board);

    try expectEqual(10, board.width);
    try expectEqualSlices(u8, "##########", board.buf[0..10]);
    try expectEqualSlices(u8, "##@.[]..##", board.buf[10..20]);
    try expectEqualSlices(u8, "##[]..[]##", board.buf[20..30]);
    try expectEqualSlices(u8, "##########", board.buf[30..40]);

    try expectEqualSlices(i8, &[_]i8 { 1, 10, -10, -1 }, board.dir);
}
const std = @import("std");
const ArrayList = std.ArrayList;

const in = std.io.getStdIn().reader();
const out = std.io.getStdOut().writer();

const Board = struct {
    width: usize,
    height: usize,
    buf: []u8,
    allocator: std.mem.Allocator,

    fn init(allocator: std.mem.Allocator, width: usize, buf: []const u8) !Board {
        return Board {
            .width = width,
            .height = buf.len / width,
            .buf = try allocator.dupe(u8, buf),
            .allocator = allocator
        };
    }

    fn clone(self: Board) !Board {
        return try Board.init(self.allocator, self.width, self.buf);
    }

    fn deinit(self: Board) void {
        self.allocator.free(self.buf);
    }

    fn get(self: Board, pos: usize) u8 {
        return self.buf[pos];
    }

    fn set(self: Board, pos: usize, v: u8) void {
        self.buf[pos] = v;
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

    const width: i32 = @intCast(board.width);
    var pos = find(board, '^').?;
    var delta: i32 = -width;

    while (true) {
        board.set(pos, 'X');
        const pos_cast: i32 = @intCast(pos);
        const new_pos = pos_cast + delta;
        switch (board.get(@intCast(new_pos))) {
            '.', 'X' => pos = @intCast(new_pos),
            '#' => delta = turn(delta, width),
            'O' => break,
            else => unreachable
        }
    }

    const steps = count(board, 'X');
    try out.print("count:{}\n", .{steps});
}

fn part2(allocator: std.mem.Allocator) !void {
    const board = try parseInput(allocator, in);
    defer board.deinit();

    const width: i32 = @intCast(board.width);
    const pos: i32 = @intCast(find(board, '^').?);
    const delta: i32 = -width;

    const new_obstructions = try findLoop(board, pos, delta, width, true);

    try out.print("count:{}\n", .{new_obstructions});
}

fn findLoop(board: Board, starting_pos: i32, starting_delta: i32, width: i32, more_loops: bool) !usize {
    var piece: u8 = ' ';
    var pos = starting_pos;
    var delta = starting_delta;
    var obstructions: usize = 0;
    while(true) {
        const new_pos = pos + delta;
        piece = board.get(@intCast(new_pos));

        switch (piece) {
            '<', '>', '^', 'v', '.' => {

                const curr_dir = delta2string(delta, width);
                if (more_loops and piece == '.') {
                    const board2 = try board.clone();
                    defer board2.deinit();
                    board2.set(@intCast(new_pos), '#');
                    obstructions += try findLoop(board2, pos, delta, width, false);
                } else {
                    if (curr_dir == piece) {
                        return 1;
                    }
                }
                pos = new_pos;
                board.set(@intCast(pos), curr_dir);
            },
            'O' => break,
            '#' => {
                delta = turn(delta, width);
            },
            else => unreachable
        }
    }
    return obstructions;
}

fn parseInput(allocator: std.mem.Allocator, reader: anytype) !Board {
    var buf: [1000]u8 = undefined;
    var board = ArrayList(u8).init(allocator);
    defer board.deinit();
    var width: usize = 0;
    var height: usize = 0;
    while (try reader.readUntilDelimiterOrEof(&buf, '\n')) |line| {
        if (width == 0) {
            width = line.len + 2;
            try board.appendNTimes('O', width);
        }
        height += 1;
        try board.append('O');
        try board.appendSlice(line);
        try board.append('O');
    }
    try board.appendNTimes('O', width);

    return try Board.init(allocator, width, board.items);
}

fn count(board: Board, needle: u8) usize {
    var c: usize = 0;
    for (board.buf) |ch| {
        if (ch == needle) {
            c += 1;
        }
    }
    return c;
}

fn find(board: Board, needle: u8) ?usize {
    return std.mem.indexOfScalar(u8, board.buf, needle);
}

fn turn(curr_delta: i32, width: i32) i32 {
    if (curr_delta == -1) return -width;
    if (curr_delta == -width) return 1;
    if (curr_delta == 1) return width;
    return -1;
}

fn delta2string(curr_delta: i32, width: i32) u8 {
    if (curr_delta == -1) return '<';
    if (curr_delta == -width) return '^';
    if (curr_delta == 1) return '>';
    return 'v';
}

const expectEqual = std.testing.expectEqual;

test "parseInput" {
    const input = "...#\n..^.";
    var fbs = std.io.fixedBufferStream(input);
    const board = try parseInput(std.testing.allocator, fbs.reader());
    defer board.deinit();
    try expectEqual(6, board.width);
    try expectEqual(4, board.height);
    try expectEqual('O', board.get(0));
    try expectEqual('.', board.get(7));
    try expectEqual('#', board.get(10));
    try expectEqual('^', board.get(15));
}

test "count" {
    const input = "...#\n..^.";
    var fbs = std.io.fixedBufferStream(input);
    const board = try parseInput(std.testing.allocator, fbs.reader());
    defer board.deinit();
    try expectEqual(1, count(board, '#'));
    try expectEqual(6, count(board, '.'));
}

test "turn" {
    try expectEqual(-10, turn(-1, 10));
    try expectEqual(1, turn(-10, 10));
    try expectEqual(10, turn(1, 10));
    try expectEqual(-1, turn(10, 10));
}


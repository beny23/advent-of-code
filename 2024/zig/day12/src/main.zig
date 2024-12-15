const std = @import("std");
const ArrayList = std.ArrayList;

const in = std.io.getStdIn().reader();
const out = std.io.getStdOut().writer();

const Board = struct {
    buf: []u8,
    width: usize,
    allocator: std.mem.Allocator,

    fn init(allocator: std.mem.Allocator, width: usize, buf: []u8) !Board {
        return Board {
            .buf = try allocator.dupe(u8, buf),
            .width = width,
            .allocator = allocator
        };
    }

    fn deinit(self: *Board) void {
        self.allocator.free(self.buf);
    }
};

const Measure = struct {
    area: usize = 0,
    circumference: usize = 0,

    fn plus(self: Measure, other: Measure) Measure {
        return Measure {
            .area = self.area + other.area,
            .circumference = self.circumference + other.circumference
        };
    }
};

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();

    //try part1(allocator);
    try part2(allocator);
}

fn part1(allocator: std.mem.Allocator) !void {
    var board = try parseInput(allocator, in);
    var price: usize = 0;
    for (0..board.buf.len) |pos| {
        const curr_tile = board.buf[pos];
        if (curr_tile != '.') {
            const m = measure(&board, pos, curr_tile);
            price += m.area * m.circumference;
            std.mem.replaceScalar(u8, board.buf, ' ', '.');
        }
    }

    try out.print("Price: {}\n", .{price});
}

fn part2(allocator: std.mem.Allocator) !void {
    var board = try parseInput(allocator, in);
    var price: usize = 0;
    for (0..board.buf.len) |pos| {
        const curr_tile = board.buf[pos];
        if (curr_tile != '.') {
            const m = measure(&board, pos, curr_tile);
            const corners = countCorners(&board, ' ');
            price += m.area * corners;
            std.mem.replaceScalar(u8, board.buf, ' ', '.');
        }
    }

    try out.print("Price: {}\n", .{price});
}

fn measure(board: *Board, pos: usize, curr_tile: u8) Measure {
    if (board.buf[pos] == curr_tile) {
        board.buf[pos] = ' ';
        const m = Measure { .area = 1 };
        const sum = m
            .plus(measure(board, pos - 1, curr_tile))
            .plus(measure(board, pos + 1, curr_tile))
            .plus(measure(board, pos - board.width, curr_tile))
            .plus(measure(board, pos + board.width, curr_tile));
        return sum;
    } else if (board.buf[pos] != ' ') {
        return Measure { .circumference = 1 };
    } else {
        return Measure {};
    }
}

fn countCorners(board: *Board, search: u8) usize {
    var corners: usize = 0;
    for (0..(board.buf.len - board.width - 1)) |pos| {
        const found = countTile(board, pos, search)
            + countTile(board, pos + 1, search)
            + countTile(board, pos + board.width, search)
            + countTile(board, pos + board.width + 1, search);

        if (found == 1 or found == 3) {
            corners += 1;
        } else if (found == 2 and ((board.buf[pos] == search) == (board.buf[pos + board.width + 1] == search))) {
            corners += 2;
        }
    }
    return corners;
}

fn countTile(board: *Board, pos: usize, search: u8) usize {
    return if (board.buf[pos] == search) 1 else 0;
}

fn parseInput(allocator: std.mem.Allocator, reader: anytype) !Board {
    var buf: [1000]u8 = undefined;
    var board = ArrayList(u8).init(allocator);
    defer board.deinit();
    var width: usize = 0;
    while (try reader.readUntilDelimiterOrEof(&buf, '\n')) |line| {
        if (width == 0) {
            width = line.len + 2;
            try board.appendNTimes('.', width);
        }
        try board.append('.');
        try board.appendSlice(line);
        try board.append('.');
    }
    try board.appendNTimes('.', width);

    return try Board.init(allocator, width, board.items);
}


const expectEqual = std.testing.expectEqual;
const expectEqualSlices = std.testing.expectEqualSlices;

test "parsing" {
    const allocator = std.testing.allocator;
    const input = "AAAB\nAABB";
    var fbs = std.io.fixedBufferStream(input);
    var board = try parseInput(allocator, fbs.reader());
    defer board.deinit();

    try expectEqual(6, board.width);
    try expectEqualSlices(u8, "......", board.buf[0..6]);
    try expectEqualSlices(u8, ".AAAB.", board.buf[6..12]);
    try expectEqualSlices(u8, ".AABB.", board.buf[12..18]);
    try expectEqualSlices(u8, "......", board.buf[18..24]);
}

const std = @import("std");
const ArrayList = std.ArrayList;
const AutoHashMapUnmanaged = std.AutoHashMapUnmanaged;

const in = std.io.getStdIn().reader();
const out = std.io.getStdOut().writer();

const TileUnmanaged = struct {
    height: u8,
    rating: usize,
    peaks: ?AutoHashMapUnmanaged(usize, void)
};

const Board = struct {
    tiles: []TileUnmanaged,
    width: usize,
    allocator: std.mem.Allocator,

    fn init(allocator: std.mem.Allocator, width: usize, buf: []const u8) !Board {
        return Board {
            .tiles = try _makeTiles(allocator, buf),
            .width = width,
            .allocator = allocator
        };
    }

    fn _makeTiles(allocator: std.mem.Allocator, buf: []const u8) ![]TileUnmanaged {
        var tiles = try allocator.alloc(TileUnmanaged, buf.len);
        for (0..buf.len) |i| {
            tiles[i] = TileUnmanaged {
                .height = buf[i],
                .rating = 0,
                .peaks = null
            };
        }
        return tiles;
    }

    fn deinit(self: *Board) void {
        for (0..self.tiles.len) |i| {
            if (self.tiles[i].peaks != null) {
                self.tiles[i].peaks.?.deinit(self.allocator);
            }
        }
        self.allocator.free(self.tiles);
    }

};

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();

    var board = try parseInput(allocator, in);
    defer board.deinit();

    var score: usize = 0;
    var ratings: usize = 0;
    for (0..board.tiles.len) |pos| {
        var set = AutoHashMapUnmanaged(usize, void){};
        defer set.deinit(allocator);

        ratings += try calculateRoutes(allocator, board, pos, '0', '9', &set);

        score += set.count();
    }

    try out.print("score:{} ratings:{}\n", .{score, ratings});
}

fn parseInput(allocator: std.mem.Allocator, reader: anytype) !Board {
    var buf: [1000]u8 = undefined;
    var board = ArrayList(u8).init(allocator);
    defer board.deinit();
    var width: usize = 0;
    while (try reader.readUntilDelimiterOrEof(&buf, '\n')) |line| {
        if (width == 0) {
            width = line.len + 2;
            try board.appendNTimes('X', width);
        }
        try board.append('X');
        try board.appendSlice(line);
        try board.append('X');
    }
    try board.appendNTimes('X', width);

    return try Board.init(allocator, width, board.items);
}

fn calculateRoutes(allocator: std.mem.Allocator, board: Board, pos: usize, level: u8, maxLevel: u8, addTo: *AutoHashMapUnmanaged(usize, void)) !usize {
    var tile: *TileUnmanaged = &board.tiles[pos];
    if (tile.height == level) {
        if (tile.peaks == null) {
            var rating: usize = 0;
            tile.peaks = AutoHashMapUnmanaged(usize, void){};
            var newPeaks: *AutoHashMapUnmanaged(usize, void) = &tile.peaks.?;

            if (level == maxLevel) {
                try newPeaks.put(allocator, pos, {});
                rating = 1;
            } else {
                rating += try calculateRoutes(allocator, board, pos + 1, level + 1, maxLevel, newPeaks);
                rating += try calculateRoutes(allocator, board, pos - 1, level + 1, maxLevel, newPeaks);
                rating += try calculateRoutes(allocator, board, pos + board.width, level + 1, maxLevel, newPeaks);
                rating += try calculateRoutes(allocator, board, pos - board.width, level + 1, maxLevel, newPeaks);
            }

            tile.rating = rating;
        }

        var peaks = tile.peaks.?;
        var it = peaks.keyIterator();
        while (it.next()) |peak| {
            try addTo.put(allocator, peak.*, {});
        }

        return tile.rating;
    }

    return 0;
}

const expectEqual = std.testing.expectEqual;
const expectEqualSlices = std.testing.expectEqualSlices;

test "parseInput" {
    const input = "1234\n5678";
    var fbs = std.io.fixedBufferStream(input);
    var board = try parseInput(std.testing.allocator, fbs.reader());
    defer board.deinit();
    try expectEqual(6, board.width);
    try expectEqual(TileUnmanaged { .height = 'X', .rating = 0, .peaks = null }, board.tiles[0]);
    try expectEqual(TileUnmanaged { .height = '1', .rating = 0, .peaks = null }, board.tiles[7]);
}

test "calculateRoutes" {
    const allocator = std.testing.allocator;
    const input = "0123\n...4\n...5\n9876";
    var fbs = std.io.fixedBufferStream(input);
    var board = try parseInput(allocator, fbs.reader());
    defer board.deinit();

    var set = AutoHashMapUnmanaged(usize, void){};
    defer set.deinit(allocator);
    const ratings = try calculateRoutes(allocator, board, 7, '0', '9', &set);
    try expectEqual(1, set.count());
    try expectEqual(true, set.contains(25));
    try expectEqual(1, ratings);
}

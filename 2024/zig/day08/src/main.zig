const std = @import("std");
const HashMap = std.HashMap;
const AutoHashMapUnmanaged = std.AutoHashMapUnmanaged;
const ArrayListUnmanaged = std.ArrayListUnmanaged;
const ArrayList = std.ArrayList;

const in = std.io.getStdIn().reader();
const out = std.io.getStdOut().writer();

const Point = struct {
    x: i16,
    y: i16,

    pub const HashContext = struct {
        pub fn hash(_: HashContext, e: Point) u64 {
            const x: u64 = @intCast(e.x);
            const y: u64 = @intCast(e.y);
            return @intCast(x * 1337 + y);
        }
        pub fn eql(_: HashContext, a: Point, b: Point) bool {
            return a.x == b.x and a.y == b.y;
        }
    };

    fn init(x: i16, y: i16) Point {
        return Point { .x = x, .y = y };
    }

    fn plus(self: Point, other: Point) Point {
        return init(self.x + other.x, self.y + other.y);
    }

    fn minus(self: Point, other: Point) Point {
        return init(self.x - other.x, self.y - other.y);
    }

    fn inside(self: Point, a: Point, b: Point) bool {
        return self.x >= a.x
            and self.x < b.x
            and self.y >= a.y
            and self.y < b.y;
    }
};

const BoardUnmanaged = struct {
    antennae: AutoHashMapUnmanaged(u8, []const Point),
    size: Point,

    fn init(size: Point, antennae: AutoHashMapUnmanaged(u8, []const Point)) !BoardUnmanaged {
        return BoardUnmanaged {
            .size = size,
            .antennae = antennae
        };
    }

    fn deinit(self: *BoardUnmanaged, allocator: std.mem.Allocator) void {
        var antennae = self.*.antennae;
        var iter = antennae.valueIterator();
        while (iter.next()) |v| {
            allocator.free(v.*);
        }
        antennae.deinit(allocator);
    }
};

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();

    var board = try parseInput(allocator, in);
    defer board.deinit(allocator);
    const map = board.antennae;
    var anti_nodes = HashMap(Point, void, Point.HashContext, std.hash_map.default_max_load_percentage).init(allocator);
    defer anti_nodes.deinit();

    var iter = map.keyIterator();
    while (iter.next()) |key| {
        const nodes = map.get(key.*).?;
        const antis = try findAntiNodes_part2(allocator, nodes, board.size);
        for (antis) |anti| {
            try anti_nodes.put(anti, {});
        }
    }

    try out.print("count:{}\n", .{anti_nodes.count()});
}

fn findAntiNodes_part1(allocator: std.mem.Allocator, antennae: []const Point, size: Point) ![]const Point {
    const origin = Point.init(0, 0);
    var list = ArrayList(Point).init(allocator);
    defer list.deinit();
    for (0..antennae.len-1) |i| {
        for (i+1..antennae.len) |j| {
            const a = antennae[i];
            const b = antennae[j];

            const delta = b.minus(a);

            const n_a = a.minus(delta);
            const n_b = b.plus(delta);

            if (n_a.inside(origin, size)) {
                try list.append(n_a);
            }
            if (n_b.inside(origin, size)) {
                try list.append(n_b);
            }
        }
    }
    return try allocator.dupe(Point, list.items);
}

fn findAntiNodes_part2(allocator: std.mem.Allocator, antennae: []const Point, size: Point) ![]const Point {
    const origin = Point.init(0, 0);
    var list = ArrayList(Point).init(allocator);
    defer list.deinit();
    for (0..antennae.len-1) |i| {
        for (i+1..antennae.len) |j| {
            const a = antennae[i];
            const b = antennae[j];

            try list.append(a);
            try list.append(b);

            const delta = b.minus(a);

            var p = a;
            while (true) {
                p = p.minus(delta);
                if (p.inside(origin, size)) {
                    try list.append(p);
                } else {
                    break;
                }
            }

            p = b;
            while (true) {
                p = p.plus(delta);
                if (p.inside(origin, size)) {
                    try list.append(p);
                } else {
                    break;
                }
            }
        }
    }
    return try allocator.dupe(Point, list.items);
}

fn parseInput(allocator: std.mem.Allocator, reader: anytype) !BoardUnmanaged {
    var buf: [1000]u8 = undefined;
    var width: i16 = 0;
    var y: i16 = 0;
    var map = AutoHashMapUnmanaged(u8, ArrayListUnmanaged(Point)){};
    defer map.deinit(allocator);

    while (try reader.readUntilDelimiterOrEof(&buf, '\n')) |line| {
        if (width == 0) {
            width = @intCast(line.len);
        }
        for (line, 0..) |ch, x| {
            if (ch != '.') {
                if (map.getPtr(ch)) |list| {
                    try list.*.append(allocator, Point{.x = @intCast(x), .y = y});
                } else {
                    var list = ArrayListUnmanaged(Point){};
                    try list.append(allocator, Point{.x = @intCast(x), .y = y});
                    try map.put(allocator, ch, list);
                }
            }
        }
        y += 1;
    }

    var input = AutoHashMapUnmanaged(u8, []const Point){};
    var iter = map.keyIterator();
    while (iter.next()) |key| {
        var list = map.get(key.*).?;
        defer list.deinit(allocator);
        try input.put(allocator, key.*, try allocator.dupe(Point, list.items));
    }
    return BoardUnmanaged.init(Point { .x = width, .y = y }, input);
}

const expectEqual = std.testing.expectEqual;
const expectEqualSlices = std.testing.expectEqualSlices;

test "parsing" {
    const allocator = std.testing.allocator;
    const input = "...a..\n1..3.a";
    var fbs = std.io.fixedBufferStream(input);
    var result = try parseInput(allocator, fbs.reader());
    defer result.deinit(allocator);
    const map = result.antennae;
    try expectEqual(3, map.count());
    try expectEqual(Point { .x = 6, .y = 2 }, result.size);
    try expectEqualSlices(Point, &[_]Point{ Point{.x=0, .y=1 }}, map.get('1').?);
    try expectEqualSlices(Point, &[_]Point{ Point{.x=3, .y=1 }}, map.get('3').?);
}

test "part1 findAntiNodes" {
    const allocator = std.testing.allocator;
    const antennae = [_]Point { Point.init(2, 2), Point.init(3, 3) };
    const expected = [_]Point { Point.init(1, 1), Point.init(4, 4) };
    const actual = try findAntiNodes_part1(allocator, &antennae, Point.init(6, 6));
    defer allocator.free(actual);
    try expectEqualSlices(Point, &expected, actual);
}

test "part1 findAntiNodes2" {
    const allocator = std.testing.allocator;
    const antennae = [_]Point { Point.init(3, 3), Point.init(2, 2) };
    const expected = [_]Point { Point.init(4, 4), Point.init(1, 1) };
    const actual = try findAntiNodes_part1(allocator, &antennae, Point.init(6, 6));
    defer allocator.free(actual);
    try expectEqualSlices(Point, &expected, actual);
}

test "part1 findAntiNodes with nodes outside the board" {
    const allocator = std.testing.allocator;
    const antennae = [_]Point { Point.init(1, 1), Point.init(3, 3) };
    const expected = [_]Point { Point.init(5, 5) };
    const actual = try findAntiNodes_part1(allocator, &antennae, Point.init(6, 6));
    defer allocator.free(actual);
    try expectEqualSlices(Point, &expected, actual);
}

test "part1 findAntiNodes no nodes inside the board" {
    const allocator = std.testing.allocator;
    const antennae = [_]Point { Point.init(1, 1), Point.init(4, 4) };
    const actual = try findAntiNodes_part1(allocator, &antennae, Point.init(6, 6));
    defer allocator.free(actual);
    try expectEqual(0, actual.len);
}

test "part2 findAntiNodes" {
    const allocator = std.testing.allocator;
    const antennae = [_]Point { Point.init(1, 1), Point.init(2, 2) };
    const expected = [_]Point { Point.init(1, 1), Point.init(2, 2), Point.init(0, 0), Point.init(3, 3), Point.init(4, 4) };
    const actual = try findAntiNodes_part2(allocator, &antennae, Point.init(5, 5));
    defer allocator.free(actual);
    try expectEqualSlices(Point, &expected, actual);
}

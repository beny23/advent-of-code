const std = @import("std");
const ArrayList = std.ArrayList;
const indexOfPos = std.mem.indexOfPos;

const in = std.io.getStdIn().reader();
const out = std.io.getStdOut().writer();

const Point = struct {
    x: i32,
    y: i32
};

const Robot = struct {
    pos: Point,
    ve: Point
};

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();

    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);
    const size = Point {
        .x = try std.fmt.parseInt(i32, args[1], 10),
        .y = try std.fmt.parseInt(i32, args[2], 10)
    };

    // try part1(allocator, size);
    try part2(allocator, size);
}

fn part1(allocator: std.mem.Allocator, size: Point) !void {
    const robots = try parseInput(allocator, in);
    for (0..robots.len) |i| {
        robots[i].pos = move(robots[i].pos, robots[i].ve, size, 100);
    }
    try printRobots(robots, size);

    const counts = count(robots, size);
    try out.print("safety:{}\n", .{counts[0] * counts[1] * counts[2] * counts[3]});
}

fn part2(allocator: std.mem.Allocator, size: Point) !void {
    const robots = try parseInput(allocator, in);
    var seconds: usize = 0;
    while (true) {
        seconds += 1;
        for (0..robots.len) |i| {
            robots[i].pos = move(robots[i].pos, robots[i].ve, size, 1);
        }
        if (try findLine(allocator, robots, size)) {
            try out.print("Second {}\n", .{seconds});
            try printRobots(robots, size);
            break;
        }
    }
}

fn printRobots(robots: []const Robot, size: Point) !void {
    for (0..@intCast(size.y)) |y| {
        for (0..@intCast(size.x)) |x| {
            var found = false;
            for (robots) |robot| {
                if (robot.pos.x == x and robot.pos.y == y) {
                    found = true;
                    break;
                }
            }
            if (found) {
                try out.print("#", .{});
            } else {
                try out.print(".", .{});
            }
        }
        try out.print("\n", .{});
    }
}

fn parseInput(allocator: std.mem.Allocator, reader: anytype) ![]Robot {
    var buf: [1000]u8 = undefined;
    var list = ArrayList(Robot).init(allocator);
    defer list.deinit();

    while (try reader.readUntilDelimiterOrEof(&buf, '\n')) |line| {
        var parts = std.mem.split(u8, line, " ");
        const pos_str: []const u8 = parts.next().?;
        const ve_str: []const u8 = parts.next().?;

        const robot = Robot {
            .pos = try parsePoint(pos_str[2..]),
            .ve = try parsePoint(ve_str[2..])
        };
        try list.append(robot);
    }

    return allocator.dupe(Robot, list.items);
}

fn move(pos: Point, ve: Point, size: Point, seconds: i32) Point {
    return Point {
        .x = @mod(pos.x + seconds * ve.x, size.x),
        .y = @mod(pos.y + seconds * ve.y, size.y)
    };
}

fn count(robots: []const Robot, size: Point) [4]usize {
    const halfX = @divTrunc(size.x, 2);
    const halfY = @divTrunc(size.y, 2);

    var counts = [4]usize { 0, 0, 0, 0 };
    for (robots) |robot| {
        var i: usize = 0;
        const pos = robot.pos;
        if (pos.x != halfX and pos.y != halfY) {
            if (pos.x > halfX) {
                i += 1;
            }
            if (pos.y > halfY) {
                i += 2;
            }
            counts[i] += 1;
        }
    }
    return counts;
}

fn parsePoint(str: []const u8) !Point {
    var parts = std.mem.split(u8, str, ",");
    return Point {
        .x = try std.fmt.parseInt(i32, parts.next().?, 10),
        .y = try std.fmt.parseInt(i32, parts.next().?, 10)
    };
}

fn findLine(allocator: std.mem.Allocator, robots: []const Robot, size: Point) !bool {
    const width: usize = @intCast(size.x);
    const height: usize = @intCast(size.y);
    var buf = try allocator.alloc(u8, width * height);
    defer allocator.free(buf);

    @memset(buf, '.');

    for (robots) |robot| {
        const x: usize = @intCast(robot.pos.x);
        const y: usize = @intCast(robot.pos.y);
        buf[x + y * width] = '#';
    }

    if (std.mem.indexOf(u8, buf, "#########")) |_| {
        return true;
    }
    return false;
}

const expectEqual = std.testing.expectEqual;

test "parsePoint" {
    const expected = Point { .x = 1, .y = -2 };
    const actual = try parsePoint("1,-2");
    try expectEqual(expected, actual);
}

test "parseInput" {
    const allocator = std.testing.allocator;
    const input = "p=0,4 v=3,-3\np=6,3 v=-1,-3";
    var fbs = std.io.fixedBufferStream(input);
    const robots = try parseInput(allocator, fbs.reader());
    defer allocator.free(robots);

    try expectEqual(Robot { .pos=Point { .x=0, .y=4 }, .ve=Point { .x=3, .y=-3 }}, robots[0]);
    try expectEqual(Robot { .pos=Point { .x=6, .y=3 }, .ve=Point { .x=-1, .y=-3 }}, robots[1]);
}

test "move" {
    const expected = Point { .x=1, .y=3 };
    const actual = move(Point { .x=2, .y=4 }, Point { .x=2, .y=-3 }, Point { .x=11, .y=7 }, 5);
    try expectEqual(expected, actual);
}

test "count" {
    const robots = [_]Robot {
        Robot { .pos=Point { .x=0, .y=4 }, .ve=Point { .x=0, .y=0 } },
        Robot { .pos=Point { .x=2, .y=0 }, .ve=Point { .x=0, .y=0 } },
        Robot { .pos=Point { .x=0, .y=0 }, .ve=Point { .x=0, .y=0 } }
    };

    const expected = [4]usize { 1, 0, 1, 0 };
    const actual = count(&robots, Point { .x=5, .y=5 });
    try expectEqual(expected, actual);
}

const std = @import("std");
const ArrayList = std.ArrayList;

const in = std.io.getStdIn().reader();
const out = std.io.getStdOut().writer();

pub fn main() !void {

    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();

    //try part1();
    try part2(allocator);
}

fn part1() !void {
    var buf: [100]u8 = undefined;
    var safe: u32 = 0;
    outer: while (try in.readUntilDelimiterOrEof(&buf, '\n')) |line| {
        var values = std.mem.split(u8, line, " ");
        var prev: i32 = -1;
        var prev_delta: i32 = 0;
        while (values.next()) |value| {
            if (value.len > 0) {
                const num = try std.fmt.parseInt(i32, value, 10);
                if (prev >= 0) {
                    const delta = prev - num;
                    if (delta == 0) continue :outer;
                    if (delta > 3 or delta < -3) continue :outer;
                    if (prev_delta != 0) {
                        if ((prev_delta < 0) != (delta < 0)) continue :outer;
                    }
                    prev_delta = delta;
                }
                prev = num;
            }
        }
        safe += 1;
    }

    try out.print("Safe:{}\n", .{safe});
}

fn part2(allocator: std.mem.Allocator) !void {
    var buf: [100]u8 = undefined;
    var safe: u32 = 0;
    while (try in.readUntilDelimiterOrEof(&buf, '\n')) |line| {
        const values = try parseLine(allocator, line);
        defer allocator.free(values);
        if (try isSafe2(allocator, values)) {
            safe += 1;
        }
    }

    try out.print("Safe:{}\n", .{safe});
}

fn parseLine(allocator: std.mem.Allocator, line: []const u8) ![]i32 {
    var list = ArrayList(i32).init(allocator);
    defer list.deinit();
    var values = std.mem.split(u8, line, " ");
    while (values.next()) |value| {
        const num = try std.fmt.parseInt(i32, value, 10);
        try list.append(num);
    }
    return try allocator.dupe(i32, list.items);
}

fn isSafe(values: []const i32) bool {
    var prev: i32 = -1;
    var prev_delta: i32 = 0;
    for (values) |num| {
        if (prev >= 0) {
            const delta = prev - num;
            if (delta == 0) return false;
            if (delta > 3 or delta < -3) return false;
            if (prev_delta != 0) {
                if ((prev_delta < 0) != (delta < 0)) return false;
            }
            prev_delta = delta;
        }
        prev = num;
    }
    return true;
}

fn isSafe2(allocator: std.mem.Allocator, values: []const i32) !bool {
    if (isSafe(values)) {
        return true;
    }
    for (0..values.len) |i| {
        var sublist = ArrayList(i32).init(allocator);
        defer sublist.deinit();
        if (i > 0) {
            try sublist.appendSlice(values[0..i]);
        }
        if (i < values.len - 1) {
            try sublist.appendSlice(values[i+1..]);
        }
        if (isSafe(sublist.items)) {
            return true;
        }
    }
    return false;
}

const expect = std.testing.expect;
const eql = std.mem.eql;

test "parseLine works" {
    const actual = try parseLine(std.testing.allocator, "1 2 3");
    defer std.testing.allocator.free(actual);
    const expected = [_]i32{ 1, 2, 3 };
    try expect(eql(i32, actual, &expected));
}

test "isSafe works" {
    try expect(isSafe(&[_]i32{ 1, 2, 3 }));
}

test "isSafe fails correctly" {
    try expect(isSafe(&[_]i32{ 1, 2, 1 }) == false);
}

test "isSafe2 correctly skips a problem" {
    try expect(try isSafe2(std.testing.allocator, &[_]i32{ 1, 2, 1 }));
    try expect(try isSafe2(std.testing.allocator, &[_]i32{ 25, 28, 29, 26, 31 }));
    try expect(try isSafe2(std.testing.allocator, &[_]i32{ 30, 28, 29, 31 }));
    try expect(try isSafe2(std.testing.allocator, &[_]i32{ 88, 87, 91, 93, 94 }));
}
const std = @import("std");
const ArrayList = std.ArrayList;
const AutoHashMap = std.AutoHashMap;

const in = std.io.getStdIn().reader();
const out = std.io.getStdOut().writer();

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();

    //try part1(allocator);
    try part2(allocator);
}

fn part1(allocator: std.mem.Allocator) !void {
    const input = try parseInput(allocator, in);
    var list = input;
    for (0..25) |i| {
        const newList = try blink(allocator, list);
        try out.print("Iter {} count {}\n", .{i, newList.len});
        allocator.free(list);
        list = newList;
    }
    allocator.free(list);
}

fn parseInput(allocator: std.mem.Allocator, reader: anytype) ![]u256 {
    var buf: [1000]u8 = undefined;
    var list = ArrayList(u256).init(allocator);
    defer list.deinit();
    while (try reader.readUntilDelimiterOrEof(&buf, '\n')) |line| {
        var it = std.mem.split(u8, line, " ");
        while (it.next()) |num_str| {
            const num = try std.fmt.parseInt(u256, num_str, 10);
            try list.append(num);
        }
    }
    return allocator.dupe(u256, list.items);
}

fn blink(allocator: std.mem.Allocator, list: []u256) ![]u256 {
    var result = ArrayList(u256).init(allocator);
    defer result.deinit();

    for (list) |x| {
        if (x == 0) {
            try result.append(1);
        } else {
            const log10 = std.math.log10(x) + 1;
            if (log10 % 2 == 0) {
                const factor = std.math.pow(u256, 10, log10 / 2);
                try result.append(x / factor);
                try result.append(x % factor);
            } else {
                try result.append(x * 2024);
            }
        }
    }

    return allocator.dupe(u256, result.items);
}

fn part2(allocator: std.mem.Allocator) !void {
    const input = try parseInput(allocator, in);
    var map = AutoHashMap(u256, usize).init(allocator);
    for (input) |x| {
        try inc(&map, x, 1);
    }

    var curr_map = map;
    for (0..75) |i| {
        var new_map = AutoHashMap(u256, usize).init(allocator);

        var it = curr_map.iterator();
        while (it.next()) |kv| {
            const num = kv.key_ptr.*;
            const size = kv.value_ptr.*;
            if (num == 0) {
                try inc(&new_map, 1, size);
            } else {
                const log10 = std.math.log10(num) + 1;
                if (log10 % 2 == 0) {
                    const factor = std.math.pow(u256, 10, log10 / 2);
                    try inc(&new_map, num / factor, size);
                    try inc(&new_map, num % factor, size);
                } else {
                    try inc(&new_map, num * 2024, size);
                }
            }
        }

        curr_map.deinit();
        curr_map = new_map;

        var sum: usize = 0;
        var values = curr_map.valueIterator();
        while (values.next()) |value_ptr| {
            sum += value_ptr.*;
        }
        try out.print("Iter {} count {} sum {}\n", .{i, curr_map.count(), sum});
    }
    curr_map.deinit();
}

fn inc(map: *AutoHashMap(u256, usize), key: u256, size: usize) !void {
    const v = try map.getOrPut(key);
    if (!v.found_existing) {
        v.value_ptr.* = 0;
    }
    v.value_ptr.* += size;
}

const expectEqual = std.testing.expectEqual;
const expectEqualSlices = std.testing.expectEqualSlices;

test "parse" {
    const allocator = std.testing.allocator;
    const input = "1 2 3";
    var fbs = std.io.fixedBufferStream(input);
    const list = try parseInput(allocator, fbs.reader());
    defer allocator.free(list);
    try expectEqual(3, list.len);
    try expectEqualSlices(u256, &[_]u256 { 1, 2, 3 }, list);
}

test "blink" {
    const allocator = std.testing.allocator;
    var input = [_]u256 { 125, 17 };
    const expected = [_]u256 { 253000, 1, 7 };
    const actual = try blink(allocator, &input);
    defer allocator.free(actual);
    try expectEqualSlices(u256, &expected, actual);
}

test "inc" {
    const allocator = std.testing.allocator;
    var map = AutoHashMap(u256, usize).init(allocator);
    defer map.deinit();
    try inc(&map, 1, 1);
    try inc(&map, 1, 1);
    try expectEqual(1, map.count());
    try expectEqual(2, map.get(1).?);
}

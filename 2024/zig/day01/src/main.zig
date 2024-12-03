const std = @import("std");
const ArrayList = std.ArrayList;

const in = std.io.getStdIn().reader();
const out = std.io.getStdOut().writer();

pub fn main() !void {

    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();

    //try part1(allocator);
    try part2(allocator);
}

fn part1(allocator: std.mem.Allocator) !void {
    var lists = [_]ArrayList(u32) { ArrayList(u32).init(allocator), ArrayList(u32).init(allocator) };
    defer lists[0].deinit();
    defer lists[1].deinit();

    try parseInput(&lists);

    for (lists) |list| {
        std.mem.sort(u32, list.items, {}, std.sort.asc(u32));
    }

    var total_distance: u32 = 0;
    for (lists[0].items, lists[1].items) |e1, e2| {
        const distance = if (e1 > e2) e1 - e2 else e2 - e1;
        total_distance += distance;
    }

    try out.print("{}\n", .{total_distance});
}

fn part2(allocator: std.mem.Allocator) !void {
    var lists = [_]ArrayList(u32) { ArrayList(u32).init(allocator), ArrayList(u32).init(allocator) };
    defer lists[0].deinit();
    defer lists[1].deinit();

    try parseInput(&lists);

    var freq = std.AutoHashMap(u32, u32).init(allocator);
    defer freq.deinit();

    for (lists[1].items) |e| {
        const v = try freq.getOrPut(e);
        if (!v.found_existing) {
            v.value_ptr.* = 1;
        } else {
            v.value_ptr.* += 1;
        }
    }

    var similarity_score: u32 = 0;
    for (lists[0].items) |e| {
        const f = if (freq.get(e)) |v| v else 0;
        similarity_score += e * f;
    }

    try out.print("{}\n", .{similarity_score});
}

fn parseInput(lists: []ArrayList(u32)) !void {
    var buf: [100]u8 = undefined;
    while (try in.readUntilDelimiterOrEof(&buf, '\n')) |line| {
        var i: usize = 0;
        var values = std.mem.split(u8, line, " ");
        while (values.next()) |value| {
            if (value.len > 0) {
                const num = try std.fmt.parseInt(u32, value, 10);
                try lists[i].append(num);
                i += 1;
            }
            if (i >= 2) {
                break;
            }
        }
    }
}


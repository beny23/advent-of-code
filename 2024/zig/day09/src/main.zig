const std = @import("std");

const ArrayList = std.ArrayList;
const DoublyLinkedList = std.DoublyLinkedList;

const in = std.io.getStdIn().reader();
const out = std.io.getStdOut().writer();

const FREE: u16 = 0xFFFF;
const L = DoublyLinkedList(File);

const File = struct {
    id: u16,
    size: u16
};

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();

    // try part1(allocator);
    try part2(allocator);
}

fn part1(allocator: std.mem.Allocator) !void {
    const buf = try parseInput(allocator, in);
    defer allocator.free(buf);

    defrag(buf);

    const sum = checksum(buf);

    try out.print("sum:{}\n", .{sum});
}

fn defrag(buf: []u16) void {
    var p: usize = 0;
    var q: usize = buf.len - 1;

    outer: while (true) {
        while (buf[p] != FREE) {
            p += 1;
            if (p == buf.len) {
                break :outer;
            }
        }

        while (buf[q] == FREE) {
            if (q == 0) {
                break :outer;
            }
            q -= 1;
        }

        if (p >= q) {
            break;
        }

        buf[p] = buf[q];
        buf[q] = FREE;
    }
}

fn checksum(buf: []const u16) u64 {
    var sum: u64 = 0;
    var i: u32 = 0;
    while (true) {
        const id = buf[i];
        if (id == FREE) {
            break;
        }
        sum += i * id;
        i += 1;
    }
    return sum;
}

fn parseInput(allocator: std.mem.Allocator, reader: anytype) ![]u16 {
    var buf: [1000]u8 = undefined;
    var id: u16 = 0;
    var list = ArrayList(u16).init(allocator);
    defer list.deinit();
    var free = false;
    while (true) {
        const len = try reader.read(&buf);
        for (0..len) |i| {
            const size = buf[i] - '0';
            var val: u16 = 0;
            if (free) {
                val = FREE;
            } else {
                val = id;
                id += 1;
            }
            try list.appendNTimes(val, size);
            free = !free;
        }
        if (len < 1000) {
            break;
        }
    }
    return allocator.dupe(u16, list.items);
}

fn part2(allocator: std.mem.Allocator) !void {
    var list = try parseInput2(allocator, in);
    defer {
        var p = list.first;
        while (p) |node| {
            p = node.next;
            allocator.destroy(node);
        }
    }

    try defrag2(allocator, &list);

    const sum = checksum2(list);

    try out.print("sum:{}\n", .{sum});
}

fn defrag2(allocator: std.mem.Allocator, list: *L) !void {
    var p = list.last;

    while (p) |full| {
        p = full.prev;
        if (full.data.id != FREE) {
            var q = list.first;
            while (q) |free| {
                if (full == free) {
                    break;
                }

                q = free.next;
                if (free.data.id == FREE and full.data.size <= free.data.size) {
                    if (full.data.size < free.data.size) {
                        var gap = try allocator.create(L.Node);
                        gap.data.id = FREE;
                        gap.data.size = free.data.size - full.data.size;
                        list.insertAfter(free, gap);
                        free.data.size = full.data.size;
                    }

                    free.data.id = full.data.id;
                    full.data.id = FREE;

                    break;
                }
            }
        }
    }
}

fn checksum2(list: L) u64 {
    var sum: u64 = 0;
    var p = list.first;
    var i: u32 = 0;
    while (p) |node| : (p = node.next) {
        const id = node.data.id;
        if (id != FREE) {
            for (0..node.data.size) |_| {
                sum += id * i;
                i += 1;
            }
        } else {
            i += node.data.size;
        }
    }
    return sum;
}


fn parseInput2(allocator: std.mem.Allocator, reader: anytype) !L {
    var buf: [1000]u8 = undefined;
    var id: u16 = 0;
    var list = L{};
    var free = false;
    while (true) {
        const len = try reader.read(&buf);
        for (0..len) |i| {
            const size = buf[i] - '0';
            var val: u16 = 0;
            if (free) {
                val = FREE;
            } else {
                val = id;
                id += 1;
            }

            var node = try allocator.create(L.Node);
            node.data.id = val;
            node.data.size = size;

            list.append(node);
            free = !free;
        }
        if (len < 1000) {
            break;
        }
    }
    return list;
}

const expectEqual = std.testing.expectEqual;
const expectEqualSlices = std.testing.expectEqualSlices;

test "parsing" {
    const allocator = std.testing.allocator;
    const input = "221";
    var fbs = std.io.fixedBufferStream(input);
    const result = try parseInput(allocator, fbs.reader());
    defer allocator.free(result);
    try expectEqualSlices(u16, &[_]u16 { 0, 0, FREE, FREE, 1 }, result);
}

test "defrag" {
    var buf = [_]u16 { 0, 0, FREE, FREE, 1 };
    const expected = [_]u16 { 0, 0, 1, FREE, FREE };
    defrag(&buf);
    try expectEqualSlices(u16, &expected, &buf);
}

test "checksum" {
    const buf = [_]u16 { 0, 0, 1, FREE };
    try expectEqual(2, checksum(&buf));
}

test "part2 parsing" {
    const allocator = std.testing.allocator;
    const input = "221";
    var fbs = std.io.fixedBufferStream(input);
    var list = try parseInput2(allocator, fbs.reader());

    var node = list.popFirst().?;
    try expectEqual(File{.id=0, .size=2}, node.data);
    allocator.destroy(node);

    node = list.popFirst().?;
    try expectEqual(File{.id=FREE, .size=2}, node.data);
    allocator.destroy(node);

    node = list.popFirst().?;
    try expectEqual(File{.id=1, .size=1}, node.data);
    allocator.destroy(node);
}

test "part2 defrag" {
    const allocator = std.testing.allocator;
    const input = "221";
    var fbs = std.io.fixedBufferStream(input);
    var list = try parseInput2(allocator, fbs.reader());

    try defrag2(allocator, &list);

    var node = list.popFirst().?;
    try expectEqual(File{.id=0, .size=2}, node.data);
    allocator.destroy(node);

    node = list.popFirst().?;
    try expectEqual(File{.id=1, .size=1}, node.data);
    allocator.destroy(node);

    node = list.popFirst().?;
    try expectEqual(File{.id=FREE, .size=1}, node.data);
    allocator.destroy(node);

    node = list.popFirst().?;
    try expectEqual(File{.id=FREE, .size=1}, node.data);
    allocator.destroy(node);
}

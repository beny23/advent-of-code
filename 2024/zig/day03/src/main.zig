const std = @import("std");
const indexOfPos = std.mem.indexOfPos;
const lastIndexOf = std.mem.lastIndexOf;
const Instr = struct { pos: usize, x: i32, y: i32 };

const in = std.io.getStdIn().reader();
const out = std.io.getStdOut().writer();

pub fn main() !void {
    //try part1();
    try part2();
}

fn part1() !void {
    var buf: [20000]u8 = undefined;
    var total: i32 = 0;
    while (try in.readUntilDelimiterOrEof(&buf, '\n')) |line| {
        var pos: usize = 0;
        while (findFirst(line, pos)) |instr| {
            total += instr.x * instr.y;
            pos = instr.pos;
        }
    }
    try out.print("total:{}\n", .{total});
}

fn part2() !void {
    var buf: [20000]u8 = undefined;
    var total: i32 = 0;
    var lastDo: usize = 0;
    var lastDont: usize = 0;
    var offset: usize = 0;
    while (try in.readUntilDelimiterOrEof(&buf, '\n')) |line| {
        var pos: usize = 0;
        while (findFirst(line, pos)) |instr| {
            pos = instr.pos;

            if (lastIndexOf(u8, line[0..pos], "do()")) |last| {
                if (lastDo < last + offset) {
                    lastDo = last + offset;
                }
            }
            if (lastIndexOf(u8, line[0..pos], "don't()")) |last| {
                if (lastDont < last + offset) {
                    lastDont = last + offset;
                }
            }

            if (lastDont <= lastDo) {
                total += instr.x * instr.y;
            }
        }

        offset += line.len;
    }
    try out.print("total:{}\n", .{total});
}

fn findFirst(line: []const u8, p: usize) ?Instr {
    if (indexOfPos(u8, line, p, "mul(")) |p_mul| {
        if (indexOfPos(u8, line, p_mul, ")")) |p_close| {
            const candidate = line[p_mul+4..p_close];
            var strs = std.mem.split(u8, candidate, ",");
            if (strs.next()) |s1| {
                if (strs.next()) |s2| {
                    const x = parseNext(s1);
                    const y = parseNext(s2);
                    if (x != -1 and y != -1 and strs.peek() == null) {
                        return Instr{
                            .pos = p_close,
                            .x = x,
                            .y = y
                        };
                    }
                }
            }
        }
        return findFirst(line, p_mul+4);
    }
    return null;
}

fn parseNext(str: []const u8) i32 {
    return std.fmt.parseInt(i32, str, 10) catch -1;
}

const expect = std.testing.expect;
const expectEqual = std.testing.expectEqual;

test "findFirst works" {
    try expectEqual(findFirst("mul(1,2)", 0), Instr{.pos = 7, .x = 1, .y = 2});
    try expectEqual(findFirst("^^^^^^^^mul(1,2)^^^^^^^", 0), Instr{.pos = 15, .x = 1, .y = 2});
    try expectEqual(findFirst("mul()^^^^mul(1,2)^^^^^^^", 0), Instr{.pos = 16, .x = 1, .y = 2});
    try expectEqual(findFirst("mul(a,b)^^^^mul(1,2)^^^^^^^", 0), Instr{.pos = 19, .x = 1, .y = 2});
    try expectEqual(findFirst("mul(1,)^^^^mul(1,2)^^^^^^^", 0), Instr{.pos = 18, .x = 1, .y = 2});
    try expectEqual(findFirst("mul(,1)^^^^mul(1,2)^^^^^^^", 0), Instr{.pos = 18, .x = 1, .y = 2});
}

test "findFirst fails correctly" {
    try expect(findFirst("", 0) == null);
    try expect(findFirst("mul(a,b)^^", 0) == null);
    try expect(findFirst("mul(1,)", 0) == null);
}

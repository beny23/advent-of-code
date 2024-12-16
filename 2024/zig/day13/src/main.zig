const std = @import("std");
const ArrayList = std.ArrayList;
const indexOfPos = std.mem.indexOfPos;

const in = std.io.getStdIn().reader();
const out = std.io.getStdOut().writer();

const Machine = struct {
    buttons: [2][2]i64 = .{ .{ 0, 0 }, .{ 0, 0 }},
    prize: [2]i64 = .{ 0, 0 }
};

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();

    const machines = try parseInput(allocator, in);

    var tokens: i64 = 0;
    for (machines) |machine| {
        if (solve(machine)) |solved| {
            tokens += 3 * solved[0] + solved[1];
        }
    }

    try out.print("Tokens:{}\n", .{tokens});
}

fn solve(machine: Machine) ?[2]i64 {
    const mat = machine.buttons;
    const det: i64 = mat[0][0] * mat[1][1] - mat[0][1] * mat[1][0];
    if (det == 0) {
        return null;
    }
    const inv = [_][2]i64 {
        .{ mat[1][1], -mat[1][0] },
        .{ -mat[0][1], mat[0][0] }
    };
    // part 1: set to 0
    const adjustment: i64 = 10000000000000;
    const prize = [_]i64 { machine.prize[0] + adjustment, machine.prize[1] + adjustment };
    const solved = mul(inv, prize);
    if (@rem(solved[0], det) != 0 or @rem(solved[1], det) != 0) {
        return null;
    }
    return [_]i64 { @divExact(solved[0], det), @divExact(solved[1], det) };
}

fn mul(mat: [2][2]i64, vec: [2]i64) [2]i64 {
    return [2]i64 { mat[0][0] * vec[0] + mat[0][1] * vec[1],  mat[1][0] * vec[0] + mat[1][1] * vec[1] };
}

fn parseInput(allocator: std.mem.Allocator, reader: anytype) ![]const Machine {
    var buf: [1000]u8 = undefined;
    var list = ArrayList(Machine).init(allocator);
    defer list.deinit();

    var machine = Machine{};
    while (try reader.readUntilDelimiterOrEof(&buf, '\n')) |line| {
        if (indexOfPos(u8, line, 0, "Button A: ")) |_| {
            const a_start: usize = 12;
            const a_end = indexOfPos(u8, line, a_start, ", ").?;
            const b_start = a_end + 4;
            machine.buttons[0][0] = try std.fmt.parseInt(i64, line[a_start..a_end], 10);
            machine.buttons[0][1] = try std.fmt.parseInt(i64, line[b_start..], 10);
        } else if (indexOfPos(u8, line, 0, "Button B: ")) |_| {
            const a_start: usize = 12;
            const a_end = indexOfPos(u8, line, a_start, ", ").?;
            const b_start = a_end + 4;
            machine.buttons[1][0] = try std.fmt.parseInt(i64, line[a_start..a_end], 10);
            machine.buttons[1][1] = try std.fmt.parseInt(i64, line[b_start..], 10);
        } else if (indexOfPos(u8, line, 0, "Prize: ")) |_| {
            const a_start: usize = 9;
            const a_end = indexOfPos(u8, line, a_start, ", ").?;
            const b_start = a_end + 4;
            machine.prize[0] = try std.fmt.parseInt(i64, line[a_start..a_end], 10);
            machine.prize[1] = try std.fmt.parseInt(i64, line[b_start..], 10);

            try list.append(machine);
        }
    }

    return allocator.dupe(Machine, list.items);
}

const expectEqual = std.testing.expectEqual;

test "mul" {
    const mat = [_][2]i64 {
        .{ 3, 7 },
        .{ 4, 5 }
    };
    const vec = [_]i64 { 2, 9 };
    const expected = [_]i64 { 69, 53 };
    const actual = mul(mat, vec);
    try expectEqual(expected, actual);
}

test "parseInput" {
    const allocator = std.testing.allocator;
    const input = "Button A: X+94, Y+34\nButton B: X+22, Y+67\nPrize: X=8400, Y=5400";
    var fbs = std.io.fixedBufferStream(input);
    const machines = try parseInput(allocator, fbs.reader());
    defer allocator.free(machines);

    const expected = Machine {
        .buttons = .{
            .{ 94, 34 },
            .{ 22, 67 }},
        .prize = .{ 8400, 5400 }
    };

    try expectEqual(1, machines.len);
    try expectEqual(expected, machines[0]);
}


const std = @import("std");
const ArrayList = std.ArrayList;

const in = std.io.getStdIn().reader();
const out = std.io.getStdOut().writer();

const Calculation = struct {
    sum: u128,
    nums: []const u128,
    allocator: std.mem.Allocator,

    fn init(allocator: std.mem.Allocator, sum: u128, nums: []const u128) !Calculation {
        return Calculation{
            .sum = sum,
            .nums = try allocator.dupe(u128, nums),
            .allocator = allocator
        };
    }

    fn deinit(self: Calculation) void {
        self.allocator.free(self.nums);
    }
};

const Calculations = struct {
    calculations: []const Calculation,
    allocator: std.mem.Allocator,

    fn init(allocator: std.mem.Allocator, calculations: []const Calculation) !Calculations {
        return Calculations {
            .calculations = try allocator.dupe(Calculation, calculations),
            .allocator = allocator
        };
    }

    fn deinit(self: Calculations) void {
        for (self.calculations) |calculation| {
            calculation.deinit();
        }
        self.allocator.free(self.calculations);
    }
};

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();

    const input = try parseInput(allocator, in);
    defer input.deinit();

    var total: u128 = 0;
    for (input.calculations) |calculation| {
        const sum = calculation.sum;
        const nums = calculation.nums;

        if (isValid_part2(sum, nums[0], nums[1..])) {
            total += sum;
        }
    }
    try out.print("total:{}\n", .{total});
}

fn isValid_part1(sum: u128, acc: u128, nums: []const u128) bool {
    if (nums.len == 0) {
        return sum == acc;
    }
    return isValid_part1(sum, acc + nums[0], nums[1..])
        or isValid_part1(sum, acc * nums[0], nums[1..]);
}

fn isValid_part2(sum: u128, acc: u128, nums: []const u128) bool {
    if (nums.len == 0) {
        return sum == acc;
    }
    return isValid_part2(sum, acc + nums[0], nums[1..])
        or isValid_part2(sum, acc * nums[0], nums[1..])
        or isValid_part2(sum, concat(acc, nums[0]), nums[1..]);
}

fn concat(a: u128, b: u128) u128 {
    const log10 = std.math.log10(b) + 1;
    const factor10 = std.math.pow(u128, 10, log10);
    return a * factor10 + b;
}

fn parseInput(allocator: std.mem.Allocator, reader: anytype) !Calculations {
    var buf: [1000]u8 = undefined;
    var calculations = ArrayList(Calculation).init(allocator);
    defer calculations.deinit();

    while (try reader.readUntilDelimiterOrEof(&buf, '\n')) |line| {
        var values = std.mem.split(u8, line, ": ");
        const sum = try std.fmt.parseInt(u128, values.next().?, 10);
        const num_str = values.next().?;

        var nums = ArrayList(u128).init(allocator);
        defer nums.deinit();

        var num_strs = std.mem.split(u8, num_str, " ");
        while (num_strs.next()) |str| {
            try nums.append(try std.fmt.parseInt(u128, str, 10));
        }

        const calculation = try Calculation.init(allocator, sum, nums.items);
        try calculations.append(calculation);
    }

    return try Calculations.init(allocator, calculations.items);
}

const expectEqual = std.testing.expectEqual;
const expectEqualSlices = std.testing.expectEqualSlices;

test "parseInput" {
    const input = "10: 1 2";
    var fbs = std.io.fixedBufferStream(input);
    const result = try parseInput(std.testing.allocator, fbs.reader());
    defer result.deinit();
    try expectEqual(1, result.calculations.len);
    try expectEqual(10, result.calculations[0].sum);
    try expectEqualSlices(u128, &[_]u128{ 1, 2 }, result.calculations[0].nums);
}

test "parseInput2" {
    const input = "10: 1 2\n5: 1 2 3";
    var fbs = std.io.fixedBufferStream(input);
    const result = try parseInput(std.testing.allocator, fbs.reader());
    defer result.deinit();
    try expectEqual(2, result.calculations.len);
    try expectEqual(10, result.calculations[0].sum);
    try expectEqualSlices(u128, &[_]u128{ 1, 2 }, result.calculations[0].nums);
    try expectEqual(5, result.calculations[1].sum);
    try expectEqualSlices(u128, &[_]u128{ 1, 2, 3 }, result.calculations[1].nums);
}

test "isValid_part1" {
    try expectEqual(true, isValid_part1(190, 10, &[_]u128{ 19 }));
    try expectEqual(true, isValid_part1(3267, 81, &[_]u128{ 40, 27 }));
}

test "isValid_part2" {
    try expectEqual(true, isValid_part2(190, 10, &[_]u128{ 19 }));
    try expectEqual(true, isValid_part2(3267, 81, &[_]u128{ 40, 27 }));
    try expectEqual(true, isValid_part2(156, 15, &[_]u128{ 6 }));
    try expectEqual(true, isValid_part2(192, 17, &[_]u128{ 8, 14 }));
}

test "concat" {
    try expectEqual(124, concat(12, 4));
    try expectEqual(92, concat(9, 2));
    try expectEqual(82, concat(8, 2));
    try expectEqual(1001, concat(100, 1));
    try expectEqual(10001, concat(1000, 1));
    try expectEqual(1244466, concat(12444, 66));
}

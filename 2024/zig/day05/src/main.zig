const std = @import("std");
const indexOfScalar = std.mem.indexOfScalar;
const ArrayList = std.ArrayList;

const Rule = struct { before: u16, after: u16 };
const Pages = []const u16;
const RuleSet = struct {
    rules: []const Rule,
    pages: []const Pages,
    allocator: std.mem.Allocator,

    fn init(allocator: std.mem.Allocator, rules: []const Rule, pages: []const Pages) !RuleSet {
        return RuleSet{
            .rules = try allocator.dupe(Rule, rules),
            .pages = try allocator.dupe(Pages, pages),
            .allocator = allocator
        };
    }

    fn deinit(self: RuleSet) void {
        self.allocator.free(self.rules);
        for (self.pages) |page| {
            self.allocator.free(page);
        }
        self.allocator.free(self.pages);
    }
};

const in = std.io.getStdIn().reader();
const out = std.io.getStdOut().writer();

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();

    //try part1(allocator);
    try part2(allocator);
}

fn part1(allocator: std.mem.Allocator) !void {
    const ruleSet = try parseInput(allocator, in);
    defer ruleSet.deinit();

    var sum: u32 = 0;
    for (ruleSet.pages) |pages| {
        if (isValid(pages, ruleSet.rules)) {
            sum += pages[pages.len / 2];
        }
    }

    try out.print("sum:{}\n", .{sum});
}

fn part2(allocator: std.mem.Allocator) !void {
    const ruleSet = try parseInput(allocator, in);
    defer ruleSet.deinit();

    var sum: u32 = 0;
    for (ruleSet.pages) |pages| {
        if (!isValid(pages, ruleSet.rules)) {
            const sorted: []u16 = try allocator.dupe(u16, pages);
            defer allocator.free(sorted);
            sort(sorted, ruleSet.rules);
            sum += sorted[sorted.len / 2];
        }
    }

    try out.print("sum:{}\n", .{sum});
}

fn isValid(pages: Pages, rules: []const Rule) bool {
    for (rules) |rule| {
        if (indexOfScalar(u16, pages, rule.before)) |before| {
            if (indexOfScalar(u16, pages, rule.after)) |after| {
                if (before > after) {
                    return false;
                }
            }
        }
    }
    return true;
}

fn sort(pages: []u16, rules: []const Rule) void {
    std.mem.sort(u16, pages, rules, comptime compByRules);
}

fn compByRules(context: []const Rule, a: u16, b: u16) bool {
    for (context) |rule| {
        if (rule.before == a and rule.after == b) {
            return true;
        }
    }
    return false;
}

fn parseInput(allocator: std.mem.Allocator, reader: anytype) !RuleSet {
    var buf: [1000]u8 = undefined;
    var rules = ArrayList(Rule).init(allocator);
    defer rules.deinit();
    var pages = ArrayList(Pages).init(allocator);
    defer pages.deinit();
    var parsingRules = true;

    while (try reader.readUntilDelimiterOrEof(&buf, '\n')) |line| {

        if (parsingRules and line.len == 0) {
            parsingRules = false;
            continue;
        }

        if (parsingRules) {
            var values = std.mem.split(u8, line, "|");
            const rule = Rule{
                .before = try std.fmt.parseInt(u16, values.next().?, 10),
                .after = try std.fmt.parseInt(u16, values.next().?, 10)
            };
            try rules.append(rule);
        } else {
            var values = std.mem.split(u8, line, ",");

            var pageNumbers = ArrayList(u16).init(allocator);
            defer pageNumbers.deinit();

            while (values.next()) |value| {
                try pageNumbers.append(try std.fmt.parseInt(u16, value, 10));
            }

            try pages.append(try allocator.dupe(u16, pageNumbers.items));
        }
    }

    return RuleSet.init(allocator, rules.items, pages.items);
}

const expectEqual = std.testing.expectEqual;
const expectEqualSlices = std.testing.expectEqualSlices;

test "parseInput" {
    var fbs = std.io.fixedBufferStream("1|2\n\n1,2,3");
    const ruleSet = try parseInput(std.testing.allocator, fbs.reader());
    defer ruleSet.deinit();

    try expectEqual(1, ruleSet.rules.len);
    try expectEqual(Rule{.before=1, .after=2}, ruleSet.rules[0]);
    try expectEqual(1, ruleSet.pages.len);
    try expectEqualSlices(u16, &[_]u16{1,2,3}, ruleSet.pages[0]);
}

test "parseInput2" {
    var fbs = std.io.fixedBufferStream("1|2\n2|3\n\n1,2,3\n4,5");
    const ruleSet = try parseInput(std.testing.allocator, fbs.reader());
    defer ruleSet.deinit();

    try expectEqual(2, ruleSet.rules.len);
    try expectEqual(Rule{.before=1, .after=2}, ruleSet.rules[0]);
    try expectEqual(Rule{.before=2, .after=3}, ruleSet.rules[1]);
    try expectEqual(2, ruleSet.pages.len);
    try expectEqualSlices(u16, &[_]u16{1,2,3}, ruleSet.pages[0]);
    try expectEqualSlices(u16, &[_]u16{4,5}, ruleSet.pages[1]);
}

test "isValid" {
    try expectEqual(true, isValid(&[_]u16{1,2}, &[_]Rule{Rule{.before=1,.after=2}}));
    try expectEqual(false, isValid(&[_]u16{1,2}, &[_]Rule{Rule{.before=2,.after=1}}));
}

test "sort" {
    var arr = [_]u16{2,1};
    sort(&arr, &[_]Rule{Rule{.before=1,.after=2}});
    try expectEqualSlices(u16, &[_]u16{1,2}, &arr);
}

use std::io::prelude::*;
use std::io;

fn main() {
    step2();
}

fn step1() {
    let heights = parse();
    let dim = heights.len();
    let mut visible = vec![vec![false; dim]; dim];
    trace_trees(&heights, &mut visible, north);
    trace_trees(&heights, &mut visible, west);
    trace_trees(&heights, &mut visible, east);
    trace_trees(&heights, &mut visible, south);
    println!("{}", count(&visible));
}

fn step2() {
    let heights = parse();
    let dim = heights.len();
    let mut max_score = 0;
    for x in 1..(dim-1) {
        for y in 1..(dim-1) {
            let height = heights[x][y];

            let mut north_wards = 0;
            let mut i = x;
            while i != 0 {
                north_wards += 1;
                i -= 1;
                if heights[i][y] >= height {
                    break;
                }
            }

            let mut west_wards = 0;
            let mut i = y;
            while i != 0 {
                west_wards += 1;
                i -= 1;
                if heights[x][i] >= height {
                    break;
                }
            }

            let mut south_wards = 0;
            for i in (x+1)..dim {
                south_wards += 1;
                if heights[i][y] >= height {
                    break;
                }
            }

            let mut east_wards = 0;
            for i in (y+1)..dim {
                east_wards += 1;
                if heights[x][i] >= height {
                    break;
                }
            }

            let score = north_wards * west_wards * south_wards * east_wards;
            if score > max_score {
                max_score = score;
            }
        }
    }
    println!("{}", max_score);
}

fn north(_: usize, i: usize, j: usize) -> (usize, usize) {
    (i, j)
}

fn west(_: usize, i: usize, j: usize) -> (usize, usize) {
    (j, i)
}

fn south(dim: usize, i: usize, j: usize) -> (usize, usize) {
    (i, dim - j - 1)
}

fn east(dim: usize, i: usize, j: usize) -> (usize, usize) {
    (dim - j - 1, i)
}

fn trace_trees(heights: &Vec<Vec<u32>>, visible: &mut Vec<Vec<bool>>, f: fn(usize, usize, usize) -> (usize, usize)) {
    let dim = heights.len();
    for i in 0..dim {
        let mut prev: u32 = u32::MAX;
        for j in 0..dim {
            let (x, y) = f(dim, i, j);
            let height = heights[x][y];
            if prev == u32::MAX || height > prev {
                visible[x][y] = true;
                prev = height;
            }
        }
    }
}

fn parse() -> Vec<Vec<u32>> {
    let grid: Vec<Vec<u32>> = io::stdin().lock().lines().map(|line| {
        let row: Vec<u32> = line.unwrap().chars().map(|c| c.to_digit(10)).flatten().collect();
        row
    }).collect();
    grid
}

fn count(visible: &Vec<Vec<bool>>) -> usize {
    visible.iter().map(|r| r.iter().filter(|v| **v).count()).sum::<usize>()
}
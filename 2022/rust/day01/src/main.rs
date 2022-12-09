use std::io::prelude::*;
use std::io;
use std::cmp;

fn main() {
    step2();
}

fn step1() {
    let x = io::stdin().lock().lines().fold((0, 0), |(local, max), line| {
        match line.unwrap().as_str() {
            "" => (0, cmp::max(local, max)),
            val => (local + val.parse::<u32>().unwrap(), max)
        }
    });
    println!("{}", x.1);
}

fn step2() {
    let mut sum: u32 = 0;
    let mut list: Vec<u32> = vec![];
    for line in io::stdin().lock().lines() {
        match line.unwrap().as_str() {
            "" => {
                list.push(sum);
                sum = 0;
            },
            val => {
                sum = sum + val.parse::<u32>().unwrap();
            }
        }
    }
    list.sort();
    println!("{}", list.iter().rev().take(3).sum::<u32>());
}
use std::io::prelude::*;
use std::io;

fn main() {
    step2();
}

fn step1() {
    let x = io::stdin().lock().lines().fold(0, |total, line| {
        total + match line.unwrap().as_str() {
            "A X" => 3 + 1,
            "A Y" => 6 + 2,
            "A Z" => 0 + 3,
            "B X" => 0 + 1,
            "B Y" => 3 + 2,
            "B Z" => 6 + 3,
            "C X" => 6 + 1,
            "C Y" => 0 + 2,
            "C Z" => 3 + 3,
            _ => panic!("invalid combo")
        }
    });
    println!("{}", x);
}

fn step2() {
    let x = io::stdin().lock().lines().fold(0, |total, line| {
        total + match line.unwrap().as_str() {
            "A X" => 0 + 3,
            "A Y" => 3 + 1,
            "A Z" => 6 + 2,
            "B X" => 0 + 1,
            "B Y" => 3 + 2,
            "B Z" => 6 + 3,
            "C X" => 0 + 2,
            "C Y" => 3 + 3,
            "C Z" => 6 + 1,
            _ => panic!("invalid combo")
        }
    });
    println!("{}", x);
}

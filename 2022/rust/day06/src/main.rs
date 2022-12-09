use std::collections::HashSet;
use std::io::prelude::*;
use std::io;

fn main() {
    step(14);
}

fn step(n: usize) {
    for line in io::stdin().lock().lines() {
        let signal = line.unwrap();
        let chars: Vec<char> = signal.chars().collect();
        for (i, w) in chars.windows(n).enumerate() {
            let uniq: HashSet<char> = w.iter().map(|c| *c).collect();
            if uniq.len() == n {
                println!("{}", i + n);
                break;
            }
        }
    }
}
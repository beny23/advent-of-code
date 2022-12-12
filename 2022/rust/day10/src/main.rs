use std::str::FromStr;
use std::io::prelude::*;
use std::io;
use std::cmp::max;

#[derive(Debug, PartialEq)]
struct Instruction {
    op: String,
    delta: i32,
    cycles: u8
}

impl FromStr for Instruction {
    type Err = std::num::ParseIntError;

    fn from_str(input: &str) -> Result<Self, Self::Err> {
        Ok(Instruction {
            op: input.to_string(),
            delta: input.split_once(' ').map(|v| i32::from_str(v.1).unwrap()).unwrap_or(0),
            cycles: if input == "noop" { 1 } else { 2 }
        })
    }
}

impl Instruction {
    fn new(delta: i32) -> Instruction {
        Instruction { op: String::from(""), delta: delta, cycles: if delta != 0 { 2 } else { 1 } }
    }
}

fn parse() -> Vec<Instruction> {
    let instructions: Vec<Instruction> = io::stdin().lock().lines()
        .map(|line| {
            let l = line.unwrap();
            Instruction::from_str(&l).unwrap()
        })
        .collect();
    instructions
}

fn make_deltas(instructions: &Vec<Instruction>) -> Vec<i32> {
    let mut deltas = vec![0; instructions.len() * 2];
    let mut i: usize = 1;
    for instruction in instructions.iter() {
        deltas[i + 1] += instruction.delta;
        i += usize::from(instruction.cycles);
    }
    deltas
}

fn make_signal_strength(deltas: &Vec<i32>) -> Vec<i32> {
    let mut strengths = vec![];
    let mut strength = 1;
    for delta in deltas {
        strength += delta;
        strengths.push(strength);
    }
    strengths
}

fn signal_score(strengths: &Vec<i32>, idx: usize) -> i32 {
    idx as i32 * strengths[idx - 1]
}

fn main() {
    step2();
}

fn step1() {
    let instructions = parse();
    let deltas = make_deltas(&instructions);
    let strengths = make_signal_strength(&deltas);
    let scores: Vec<i32> = vec![20_usize, 60, 100, 140, 180, 220].iter().map(|idx| signal_score(&strengths, *idx)).collect();
    println!("{}", scores.iter().sum::<i32>());
}

fn step2() {
    let instructions = parse();
    let deltas = make_deltas(&instructions);
    let strengths = make_signal_strength(&deltas);
    for (i, line) in strengths.chunks(40).enumerate() {
        for (i, strength) in line.iter().enumerate() {
            let pos = max(*strength as usize, 1);
            let pixel = if i >= pos-1 && i <= pos+1 {
                '#'
            } else {
                '.'
            };
            print!("{pixel}");
        }
        println!();
    }
}

#[cfg(test)]
mod tests {
    use crate::*;

    #[test]
    fn test_instruction_from_str() {
        assert_eq!(Instruction::from_str("noop").unwrap(), Instruction { op: String::from("noop"), delta: 0, cycles: 1 });
        assert_eq!(Instruction::from_str("addx 10").unwrap(), Instruction { op: String::from("addx 10"), delta: 10, cycles: 2 });
        assert_eq!(Instruction::from_str("addx -5").unwrap(), Instruction { op: String::from("addx -5"), delta: -5, cycles: 2 });
    }

    #[test]
    fn test_make_deltas() {
        let instructions = vec![Instruction::new(0), Instruction::new(3), Instruction::new(-5)];
        let deltas = make_deltas(&instructions);
        assert_eq!(deltas, vec![0, 0, 0, 3, 0, -5]);
    }

    #[test]
    fn test_make_signal_strengths() {
        let deltas = vec![0, 0, 1, 2, -2];
        let strengths = make_signal_strength(&deltas);
        assert_eq!(strengths, vec![1, 1, 2, 4, 2]);
    }
}
use std::str::FromStr;
use std::io::prelude::*;
use std::io;

#[derive(Debug, PartialEq)]
struct MoveInstruction {
    from: usize,
    to: usize,
    amount: usize
}

#[derive(Debug, PartialEq)]
struct ParseResult {
    instructions: Vec<MoveInstruction>,
    stacks: Vec<Vec<char>>
}

impl FromStr for MoveInstruction {
    type Err = std::num::ParseIntError;

    fn from_str(range: &str) -> Result<Self, Self::Err> {
        if let [_, amount, _, from, _, to] = range.split(' ').collect::<Vec<&str>>()[..] {
            Ok(MoveInstruction {
                from: usize::from_str(from)? - 1,
                to: usize::from_str(to)? - 1,
                amount: usize::from_str(amount)?
            })
        } else {
            panic!("need to figure out what goes here instead")
        }
    }
}

fn main() {
    step(move_crates_2);
}

fn step(f: MoveCrates) {
    let mut r = parse();
    let mut stacks = &mut r.stacks;

    for instruction in r.instructions {
        f(instruction, stacks);
    }

    for stack in stacks {
        print!("{}", stack.pop().unwrap());
    }
}

type MoveCrates = fn(MoveInstruction, &mut Vec<Vec<char>>);

fn move_crates(instruction: MoveInstruction, stacks: &mut Vec<Vec<char>>) {
    for _ in 0..instruction.amount {
        let c = stacks[instruction.from].pop().unwrap();
        stacks[instruction.to].push(c);
    }
}

fn move_crates_2(instruction: MoveInstruction, stacks: &mut Vec<Vec<char>>) {
    let s1 = &stacks[instruction.from];
    let len1 = s1.len() - instruction.amount;
    let mut c = s1[len1..].to_vec();
    stacks[instruction.to].append(&mut c);
    stacks[instruction.from].truncate(len1);
}

fn parse_stacks(crate_strs: &Vec<String>, size: usize) -> Vec<Vec<char>> {
    let mut stacks = vec![Vec::<char>::new(); size];
    for l in crate_strs.iter().rev() {
        for i in 0..size {
            if let Some(c) = l.chars().nth(1 + i*4) {
                if c != ' ' {
                    stacks[i].push(c);
                }
            }
        }
    }
    stacks
}

fn parse() -> ParseResult {
    let mut instructions: Vec<MoveInstruction> = Vec::new();
    let mut crate_strs: Vec<String> = Vec::new();
    let mut size = 0;
    for line in io::stdin().lock().lines() {
        let l = line.unwrap();
        if l.starts_with("move") {
            instructions.push(MoveInstruction::from_str(&l).unwrap());
        } else if l.starts_with(" 1") {
            size = (l.len() + 2) / 4;
        } else {
            crate_strs.push(l);
        }
    }

    ParseResult {
        instructions: instructions,
        stacks: parse_stacks(&crate_strs, size)
    }
}

#[cfg(test)]
mod tests {
    use crate::*;

    #[test]
    fn test_move_instruction_from_str() {
        assert_eq!(MoveInstruction::from_str("move 1 from 2 to 1").unwrap(), MoveInstruction { from: 1, to: 0, amount: 1 });
    }

    #[test]
    fn test_parse_stacks() {
        let crate_str = vec!(String::from("    [D]"), String::from("[N] [C]"), String::from("[Z] [M] [P]"), String::from(""));
        let expected = vec!(vec!('Z', 'N'), vec!('M', 'C', 'D'), vec!('P'));
        assert_eq!(parse_stacks(&crate_str, 3), expected);
    }

    #[test]
    fn test_move_crates() {
        let mut stacks = vec![vec!['A', 'B'], vec!['C']];
        let instruction = MoveInstruction { from: 0, to: 1, amount: 1 };
        move_crates(instruction, &mut stacks);
        let expected = vec![vec!['A'], vec!['C', 'B']];
        assert_eq!(stacks, expected);
    }

    #[test]
    fn test_move_multiple_crates() {
        let mut stacks = vec![vec!['A', 'B', 'C'], vec!['D']];
        let instruction = MoveInstruction { from: 0, to: 1, amount: 2 };
        move_crates(instruction, &mut stacks);
        let expected = vec![vec!['A'], vec!['D', 'C', 'B']];
        assert_eq!(stacks, expected);
    }

    #[test]
    fn test_move_multiple_crates_2() {
        let mut stacks = vec![vec!['A', 'B', 'C'], vec!['D']];
        let instruction = MoveInstruction { from: 0, to: 1, amount: 2 };
        move_crates_2(instruction, &mut stacks);
        let expected = vec![vec!['A'], vec!['D', 'B', 'C']];
        assert_eq!(stacks, expected);
    }
}
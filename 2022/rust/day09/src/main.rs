use std::str::FromStr;
use std::io::prelude::*;
use std::io;
use std::collections::*;

#[derive(Debug, PartialEq, Eq, Copy, Clone, Hash)]
struct Pos {
    x: i32,
    y: i32
}

impl Pos {

    fn new(x: i32, y: i32) -> Pos {
        Pos { x: x, y: y }
    }

    fn is_adjacent(self: &Self, other: &Pos) -> bool {
        (self.x - other.x).abs() <= 1 && (self.y - other.y).abs() <= 1
    }

    fn catch_up(self: &Self, other: &Pos) -> Pos {
        if self.is_adjacent(other) {
            *self
        } else {
            Pos {
                x: self.x + (other.x - self.x).signum(),
                y: self.y + (other.y - self.y).signum()
            }
        }
    }

    fn move_by(self: &Self, dir: char) -> Pos {
        match dir {
            'R' => Pos::new(self.x + 1, self.y),
            'L' => Pos::new(self.x - 1, self.y),
            'U' => Pos::new(self.x, self.y + 1),
            'D' => Pos::new(self.x, self.y - 1),
            _ => panic!("Unexpected direction")
        }
    }
}

#[derive(Debug, PartialEq)]
struct Movement {
    dir: char,
    steps: u32
}

impl Movement {
    fn new(dir: char, steps: u32) -> Movement {
        Movement {
            dir: dir,
            steps: steps
        }
    }
}

impl FromStr for Movement {
    type Err = std::num::ParseIntError;

    fn from_str(input: &str) -> Result<Self, Self::Err> {
        if let Some((dir, steps)) = input.split_once(' ') {
            Ok(Movement::new(dir.chars().next().unwrap(), u32::from_str(steps)?))
        } else {
            panic!("need to figure out what goes here instead")
        }
    }
}

#[derive(Debug, PartialEq, Copy, Clone)]
struct Rope {
    head: Pos,
    tail: Pos
}

impl Rope {
    fn new() -> Rope {
        Rope { head: Pos::new(0, 0), tail: Pos::new(0, 0) }
    }

    fn move_by(self: &Self, movement: &Movement, tail_history: &mut HashSet<Pos>) -> Rope {
        let mut head = self.head;
        let mut tail = self.tail;
        let dir = movement.dir;
        for _ in 0..movement.steps {
            head = head.move_by(dir);
            tail = tail.catch_up(&head);
            tail_history.insert(tail);
        }
        Rope {
            head: head,
            tail: tail
        }
    }
}

#[derive(Debug, PartialEq, Copy, Clone)]
struct LongRope {
    knots: [Pos; 10]
}

impl LongRope {
    fn new() -> LongRope {
        LongRope { knots: [Pos::new(0, 0); 10] }
    }

    fn move_by(self: &Self, movement: &Movement, tail_history: &mut HashSet<Pos>) -> LongRope {
        let mut knots = self.knots;
        let dir = movement.dir;
        for _ in 0..movement.steps {
            let mut iter = knots.iter_mut();
            let mut head = iter.next().unwrap();
            *head = head.move_by(dir);
            let mut prev = head;
            for knot in iter {
                *knot = knot.catch_up(prev);
                prev = knot;
            }
            tail_history.insert(*prev);
        }
        LongRope {
            knots: knots
        }
    }
}

fn main() {
    step2();
}

fn step1() {
    let instructions = parse();
    let mut rope = Rope::new();
    let mut history = HashSet::new();
    for movement in instructions {
        rope = rope.move_by(&movement, &mut history);
    }
    println!("{}", history.len());
}

fn step2() {
    let instructions = parse();
    let mut rope = LongRope::new();
    let mut history = HashSet::new();
    for movement in instructions {
        rope = rope.move_by(&movement, &mut history);
    }
    println!("{}", history.len());
}

fn parse() -> Vec<Movement> {
    let instructions: Vec<Movement> = io::stdin().lock().lines()
        .map(|line| {
            let l = line.unwrap();
            Movement::from_str(&l).unwrap()
        })
        .collect();
    instructions
}

#[cfg(test)]
mod tests {
    use crate::*;

    #[test]
    fn test_is_adjacent() {
        assert_eq!(Pos::new(1, 1).is_adjacent(&Pos::new(1, 2)), true);
        assert_eq!(Pos::new(1, 1).is_adjacent(&Pos::new(2, 1)), true);
        assert_eq!(Pos::new(1, 1).is_adjacent(&Pos::new(0, 1)), true);
        assert_eq!(Pos::new(1, 1).is_adjacent(&Pos::new(0, 0)), true);
        assert_eq!(Pos::new(1, 1).is_adjacent(&Pos::new(2, 2)), true);
        assert_eq!(Pos::new(1, 1).is_adjacent(&Pos::new(2, 3)), false);
        assert_eq!(Pos::new(1, 1).is_adjacent(&Pos::new(3, 2)), false);
        assert_eq!(Pos::new(1, 1).is_adjacent(&Pos::new(3, 1)), false);
    }

    #[test]
    fn test_catch_up() {
        assert_eq!(Pos::new(1, 1).catch_up(&Pos::new(2, 2)), Pos::new(1, 1));
        assert_eq!(Pos::new(1, 1).catch_up(&Pos::new(3, 1)), Pos::new(2, 1));
        assert_eq!(Pos::new(1, 1).catch_up(&Pos::new(1, 3)), Pos::new(1, 2));
        assert_eq!(Pos::new(1, 1).catch_up(&Pos::new(3, 2)), Pos::new(2, 2));
        assert_eq!(Pos::new(1, 1).catch_up(&Pos::new(2, 3)), Pos::new(2, 2));
    }

    #[test]
    fn test_catch_by() {
        assert_eq!(Pos::new(1, 1).move_by('U'), Pos::new(1, 2));
        assert_eq!(Pos::new(1, 1).move_by('D'), Pos::new(1, 0));
        assert_eq!(Pos::new(1, 1).move_by('R'), Pos::new(2, 1));
        assert_eq!(Pos::new(1, 1).move_by('L'), Pos::new(0, 1));
    }

    #[test]
    fn test_movement_parsing() {
        assert_eq!(Movement::from_str("R 1").unwrap(), Movement::new('R', 1));
    }

    #[test]
    fn test_rope_movement() {
        let rope = Rope::new();
        let mut history = HashSet::new();
        let rope = rope.move_by(&Movement::new('R', 2), &mut history);
        let rope = rope.move_by(&Movement::new('U', 1), &mut history);
        assert_eq!(rope.head, Pos::new(2, 1));
        assert_eq!(rope.tail, Pos::new(1, 0));
    }
}
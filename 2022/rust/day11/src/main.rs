use std::str::FromStr;
use std::io::prelude::*;
use std::io;
use std::rc::Rc;
use std::cell::RefCell;
use num::integer::lcm;

enum Op {
    Pow,
    Add(u64),
    Mul(u64)
}

struct Monkey {
    levels: Rc<RefCell<Vec<u64>>>,
    op: Op,
    test: u64,
    if_true: usize,
    if_false: usize,
    counter: Rc<RefCell<u64>>
}

fn parse() -> Vec<Monkey> {
    let mut monkeys = vec![];
    let lines: Vec<String> = io::stdin().lock().lines().map(|l| l.unwrap()).collect();
    for group in lines.chunks(7) {
        let levels = group[1][18..].split(", ").map(|v| u64::from_str(v).unwrap()).collect::<Vec<_>>();
        let test = u64::from_str(&group[3][21..]).unwrap();
        let op: Op = match group[2][23..].split_once(" ") {
            Some(("*", "old")) => Op::Pow,
            Some(("+", v)) => Op::Add(u64::from_str(v).unwrap()),
            Some(("*", v)) => Op::Mul(u64::from_str(v).unwrap()),
            _ => panic!("Invalid op")
        };
        let if_true = usize::from_str(&group[4][29..]).unwrap();
        let if_false = usize::from_str(&group[5][30..]).unwrap();
        let monkey = Monkey {
            levels: Rc::new(RefCell::new(levels)),
            op: op,
            test: test,
            if_true: if_true,
            if_false: if_false,
            counter: Rc::new(RefCell::new(0))
        };
        monkeys.push(monkey);
    }
    monkeys
}

fn main() {
    // step(3, 20);
    step(1, 10000);
}

fn step(div: u64, rounds: usize) {
    let monkeys = parse();
    let lcm = monkeys.iter().map(|m| m.test).reduce(|a, b| lcm(a, b)).unwrap();
    for _ in 0..rounds {
        for monkey in monkeys.iter() {
            let mut counter = 0;
            let mut levels = monkey.levels.borrow_mut();
            while levels.len() > 0 {
                counter += 1;
                let level = levels.remove(0);
                let new_level = match monkey.op {
                    Op::Pow => level.pow(2),
                    Op::Mul(v) => level * v,
                    Op::Add(v) => level + v
                } / div;
                let next_monkey = if new_level % monkey.test == 0 {
                    monkey.if_true
                } else {
                    monkey.if_false
                };
                monkeys[next_monkey].levels.borrow_mut().push(new_level % lcm);
            }
            *monkey.counter.borrow_mut() += counter;
        }
        for (i, monkey) in monkeys.iter().enumerate() {
            println!("Monkey {}: {:?}", i, monkey.levels.borrow());
        }
        println!();
    }
    let mut counters = monkeys.iter().map(|m| *m.counter.borrow()).collect::<Vec<_>>();
    println!("Counters: {:?}", counters);
    counters.sort();
    let monkey_business = counters.iter().rev().take(2).product::<u64>();
    println!("Monkey business: {}", monkey_business);
}

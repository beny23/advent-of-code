use std::collections::HashSet;
use std::io::prelude::*;
use std::io;

fn main() {
    step2();
}

fn step1() {
    let x = io::stdin().lock().lines().map(|line| {
        let l = line.unwrap();
        let (s1, s2) = split(l.as_str());
        let c = common(s1, s2);
        score(c)
    });
    println!("{}", x.sum::<u32>())
}

fn step2() {
    let lines: Vec<String> = io::stdin().lock().lines()
        .map(|line| line.unwrap())
        .collect();
    let x = lines
        .chunks_exact(3)
        .map(|group| {
            group.iter()
                .map(|s| s.chars().collect::<HashSet<_>>())
                .reduce(|a, b| {
                    a.intersection(&b).map(|c| *c).collect::<HashSet<_>>()
                })
                .map(|s| score(*s.iter().next().unwrap()))
                .unwrap()
        });
    println!("{}", x.sum::<u32>())
}

fn split(s: &str) -> (&str, &str) {
    let mid = s.len() / 2;
    (&s[0..mid], &s[mid..])
}

fn common(s1: &str, s2: &str) -> char {
    let chars1: HashSet<char> = s1.chars().collect();
    let chars2: HashSet<char> = s2.chars().collect();
    *chars1.intersection(&chars2).next().unwrap()
}

fn score(c: char) -> u32 {
    if c.is_lowercase() {
        c as u32 - 'a' as u32 + 1
    }  else {
        c as u32 - 'A' as u32 + 27
    }
}

#[cfg(test)]
mod tests {
    use crate::*;

    #[test]
    fn test_split() {
        assert_eq!(split("aaabbb"), ("aaa", "bbb"));
    }

    #[test]
    fn test_common() {
        assert_eq!(common("abcd", "efga"), 'a');
        assert_eq!(common("abcd", "aefga"), 'a');
    }

    #[test]
    fn test_score() {
        assert_eq!(score('p'), 16);
        assert_eq!(score('L'), 38);
        assert_eq!(score('P'), 42);
        assert_eq!(score('t'), 20);
        assert_eq!(score('s'), 19);
    }
}
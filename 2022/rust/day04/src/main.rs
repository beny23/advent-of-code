use std::str::FromStr;
use std::io::prelude::*;
use std::io;

#[derive(Debug, PartialEq)]
struct SectionRange {
    from: u16,
    to: u16
}

impl FromStr for SectionRange {
    type Err = std::num::ParseIntError;

    fn from_str(range: &str) -> Result<Self, Self::Err> {
        let (from, to) = range.split_once('-').unwrap();
        Ok(SectionRange {
            from: u16::from_str(from)?,
            to: u16::from_str(to)?
        })
    }
}

impl SectionRange {
    fn contains(self: &Self, other: &SectionRange) -> bool {
        self.from <= other.from && self.to >= other.to
    }

    fn overlaps(self: &Self, other: &SectionRange) -> bool {
        (self.from <= other.to && self.to >= other.from)
            || (other.from <= self.to && other.to >= self.from)
    }
}

#[derive(Debug, PartialEq)]
struct AssignmentPair {
    range1: SectionRange,
    range2: SectionRange
}

impl FromStr for AssignmentPair {
    type Err = std::num::ParseIntError;

    fn from_str(range: &str) -> Result<Self, Self::Err> {
        let (r1, r2) = range.split_once(',').unwrap();
        Ok(AssignmentPair {
            range1: SectionRange::from_str(r1)?,
            range2: SectionRange::from_str(r2)?
        })
    }
}

impl AssignmentPair {
    fn contained(self: &Self) -> bool {
        self.range1.contains(&self.range2) || self.range2.contains(&self.range1)
    }

    fn overlapped(self: &Self) -> bool {
        self.range1.overlaps(&self.range2)
    }
}

fn main() {
    step2();
}

fn step1() {
    let x = io::stdin().lock().lines()
        .map(|line| AssignmentPair::from_str(&line.unwrap()).unwrap())
        .filter(|pair| pair.contained())
        .count();
    println!("{}", x);
}

fn step2() {
    let x = io::stdin().lock().lines()
        .map(|line| AssignmentPair::from_str(&line.unwrap()).unwrap())
        .filter(|pair| pair.overlapped())
        .count();
    println!("{}", x);
}

#[cfg(test)]
mod tests {
    use crate::*;

    #[test]
    fn test_section_range_from_str() {
        assert_eq!(SectionRange::from_str("2-3").unwrap(), SectionRange { from: 2, to: 3 });
    }

    #[test]
    fn test_assignment_pair_from_str() {
        assert_eq!(AssignmentPair::from_str("2-3,6-8").unwrap(), AssignmentPair {
            range1: SectionRange { from: 2, to: 3 },
            range2: SectionRange { from: 6, to: 8 }
        });
    }

    #[test]
    fn test_contains() {
        let r1 = SectionRange { from: 2, to: 3 };
        let r2 = SectionRange { from: 1, to: 7 };
        assert_eq!(r1.contains(&r2), false);
        assert_eq!(r2.contains(&r1), true);
    }

    #[test]
    fn test_is_contained() {
        let pair = AssignmentPair {
            range1: SectionRange { from: 2, to: 3 },
            range2: SectionRange { from: 1, to: 7 }
        };
        assert_eq!(pair.contained(), true);

        let pair = AssignmentPair {
            range1: SectionRange { from: 1, to: 7 },
            range2: SectionRange { from: 2, to: 3 }
        };
        assert_eq!(pair.contained(), true);
    }

    #[test]
    fn test_is_not_contained() {
        let pair = AssignmentPair {
            range1: SectionRange { from: 2, to: 3 },
            range2: SectionRange { from: 4, to: 7 }
        };
        assert_eq!(pair.contained(), false);

        let pair = AssignmentPair {
            range1: SectionRange { from: 2, to: 4 },
            range2: SectionRange { from: 3, to: 7 }
        };
        assert_eq!(pair.contained(), false);
    }

    #[test]
    fn test_overlaps() {
        let r1 = SectionRange { from: 5, to: 7 };
        let r2 = SectionRange { from: 7, to: 9 };
        assert_eq!(r1.overlaps(&r2), true);
        assert_eq!(r2.overlaps(&r1), true);

        let r1 = SectionRange { from: 2, to: 8 };
        let r2 = SectionRange { from: 3, to: 7 };
        assert_eq!(r1.overlaps(&r2), true);
        assert_eq!(r2.overlaps(&r1), true);

        let r1 = SectionRange { from: 6, to: 6 };
        let r2 = SectionRange { from: 4, to: 6 };
        assert_eq!(r1.overlaps(&r2), true);
        assert_eq!(r2.overlaps(&r1), true);

        let r1 = SectionRange { from: 2, to: 6 };
        let r2 = SectionRange { from: 4, to: 8 };
        assert_eq!(r1.overlaps(&r2), true);
        assert_eq!(r2.overlaps(&r1), true);
    }

    #[test]
    fn test_dont_overlap() {
        let r1 = SectionRange { from: 2, to: 4 };
        let r2 = SectionRange { from: 6, to: 8 };
        assert_eq!(r1.overlaps(&r2), false);
        assert_eq!(r2.overlaps(&r1), false);

        let r1 = SectionRange { from: 2, to: 3 };
        let r2 = SectionRange { from: 4, to: 5 };
        assert_eq!(r1.overlaps(&r2), false);
        assert_eq!(r2.overlaps(&r1), false);
    }

    #[test]
    fn test_is_overlapped() {
        let pair = AssignmentPair {
            range1: SectionRange { from: 5, to: 7 },
            range2: SectionRange { from: 7, to: 9 }
        };
        assert_eq!(pair.overlapped(), true);
    }
}
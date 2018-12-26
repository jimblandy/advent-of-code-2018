#![feature(copy_within)]

use std::iter::repeat;

#[allow(dead_code)]
static TEST_INPUT: &'static str = include_str!("day-12.test");
#[allow(dead_code)]
static INPUT: &'static str = include_str!("day-12.input");

type Rule = [bool; 32];

#[derive(Debug)]
struct State {
    origin: isize,
    pots: Vec<bool>,
}

fn pot_to_bool(pot: char) -> bool {
    if pot == '#' {
        true
    } else if pot == '.' {
        false
    } else {
        panic!("unexpected pot character: {:?}", pot);
    }
}

fn bool_to_pot(live: bool) -> char {
    if live {
        '#'
    } else {
        '.'
    }
}

fn parse_rule_line(line: &str) -> (usize, bool) {
    assert_eq!(line.len(), 10);
    assert_eq!(&line[5..9], " => ");
    let mut index = 0;
    for pot in line.chars().take(5) {
        index <<= 1;
        index |= pot_to_bool(pot) as usize;
    }
    (index, pot_to_bool(line[9..].chars().next().unwrap()))
}

#[test]
fn test_parse_rule_line() {
    assert_eq!(parse_rule_line("..... => #"), (0b00000, true));
    assert_eq!(parse_rule_line("..... => ."), (0b00000, false));
    assert_eq!(parse_rule_line("##### => #"), (0b11111, true));
    assert_eq!(parse_rule_line("##### => ."), (0b11111, false));
    assert_eq!(parse_rule_line(".#### => #"), (0b01111, true));
    assert_eq!(parse_rule_line("###.. => #"), (0b11100, true));
}

impl State {
    fn new() -> State {
        State {
            origin: 0,
            pots: Vec::new(),
        }
    }

    fn from_input(line: &str) -> State {
        assert_eq!(&line[..15], "initial state: ");
        let mut pots = Vec::with_capacity(line.len() - 15);
        for pot in line[15..].chars() {
            pots.push(pot_to_bool(pot));
        }
        State { origin: 0, pots }
    }

    fn clear(&mut self) {
        self.pots.clear();
        self.origin = 0;
    }

    fn step(&mut self, rule: &Rule, next: &mut State) {
        // We don't support rules that allow plants to spurt up out of nowhere.
        assert_eq!(rule[0], false);

        next.pots.clear();
        next.origin = self.origin - 2;
        let mut neighborhood: usize = 0;
        for pot in self.pots.iter().cloned().chain(repeat(false).take(4)) {
            neighborhood <<= 1;
            neighborhood |= pot as usize;
            neighborhood &= 0b11111;
            next.pots.push(rule[neighborhood]);
        }

        if let Some(start) = next.pots.iter().position(|pot| *pot) {
            let end = next.pots.iter().rposition(|pot| *pot).unwrap() + 1;
            next.pots.copy_within(start..end, 0);
            next.pots.truncate(end - start);
            next.origin += start as isize;
        } else {
            next.clear();
        }
    }

    fn display(&self, left: &mut isize) {
        if self.origin < *left {
            *left = self.origin - 16;
            println!("---");
        }

        for _ in *left..self.origin {
            print!(".");
        }
        for &live in &self.pots {
            print!("{}", bool_to_pot(live))
        }
        println!();
    }

    fn count_and_sum(&self) -> (usize, usize) {
        self.pots
            .iter()
            .cloned()
            .enumerate()
            .filter(|(_, p)| *p)
            .fold((0, 0), |s, i| (s.0 + 1, s.1 + i.0))
    }

    fn code(&self) -> isize {
        let (count, sum) = self.count_and_sum();
        (self.origin * count as isize) + sum as isize
    }
}

fn main() {
    let mut lines = INPUT.lines();
    let mut state = State::from_input(&lines.next().expect("no initial state?"));
    let mut rule = [false; 32];
    for line in lines {
        if line.trim().is_empty() {
            continue;
        }
        let (index, value) = parse_rule_line(line);
        rule[index] = value;
    }

    let mut next = State::new();
    let mut left = -8;
    let mut steps = 0;
    state.display(&mut left);
    for _ in 0..500 {
        state.step(&rule, &mut next);
        steps += 1;
        if state.pots == next.pots {
            break;
        }
        std::mem::swap(&mut state, &mut next);
        state.display(&mut left);
    }
    println!();

    let (count, sum) = state.count_and_sum();
    println!("count: {}   sum: {}   code: {}", count, sum, state.code());

    if state.pots != next.pots {
        println!("Didn't converge!");
    }

    // Since we know the state has converged, the only state left to adjust per
    // step is the origin. That adjustment, we can project into the future.
    let remaining = 50_000_000_000 - steps;
    next.origin += remaining * (next.origin - state.origin);
    println!("code after 50 billion steps: {}", next.code());
}

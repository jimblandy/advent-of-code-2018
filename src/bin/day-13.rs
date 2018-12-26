#![allow(dead_code)]

extern crate ndarray;

use ndarray::{Array2, Axis};
use std::cmp::max;
use std::fmt;

#[allow(dead_code)]
static TEST_INPUT: &'static str = include_str!("day-13.test");
#[allow(dead_code)]
static INPUT: &'static str = include_str!("day-13.input");

#[derive(Clone, Copy, Debug)]
enum Legend {
    Space,
    Horizontal,
    Vertical,
    Intersection,
    LeftTurn,  // if going up
    RightTurn, // if going up
}

impl Legend {
    fn from_char(ch: char) -> (Legend, Option<Direction>) {
        match ch {
            ' ' => (Legend::Space, None),
            '/' => (Legend::RightTurn, None),
            '\\' => (Legend::LeftTurn, None),
            '-' => (Legend::Horizontal, None),
            '|' => (Legend::Vertical, None),
            '+' => (Legend::Intersection, None),
            '^' => (Legend::Vertical, Some((-1, 0))),
            'v' => (Legend::Vertical, Some((1, 0))),
            '<' => (Legend::Horizontal, Some((0, -1))),
            '>' => (Legend::Horizontal, Some((0, 1))),
            _ => panic!("Unexpected map character: {:?}", ch),
        }
    }
}

impl fmt::Display for Legend {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        f.write_str(match self {
            Legend::Space => " ",
            Legend::Horizontal => "-",
            Legend::Vertical => "|",
            Legend::Intersection => "+",
            Legend::LeftTurn => "\\",
            Legend::RightTurn => "/",
        })
    }
}

type Map = Array2<Legend>;
type Position = (usize, usize);
type Direction = (isize, isize);

fn go(pos: Position, dir: Direction) -> Position {
    (
        (pos.0 as isize + dir.0) as usize,
        (pos.1 as isize + dir.1) as usize,
    )
}

#[derive(Debug, Clone, Copy)]
enum Choice {
    Left,
    Straight,
    Right,
}

impl Choice {
    fn next(self) -> Choice {
        match self {
            Choice::Left => Choice::Straight,
            Choice::Straight => Choice::Right,
            Choice::Right => Choice::Left,
        }
    }

    fn apply(&self, dir: Direction) -> Direction {
        match self {
            Choice::Left => (-dir.1, dir.0),
            Choice::Straight => dir,
            Choice::Right => (dir.1, -dir.0),
        }
    }
}

#[derive(Debug)]
struct Cart {
    position: Position,
    direction: Direction,
    next: Choice,
}

impl Cart {
    fn step(&mut self, map: &Map) {
        match map[self.position] {
            Legend::Horizontal => {
                assert_eq!(self.direction.0, 0);
            }
            Legend::Vertical => {
                assert_eq!(self.direction.1, 0);
            }
            Legend::Intersection => {
                self.direction = self.next.apply(self.direction);
                self.next = self.next.next();
            }
            Legend::LeftTurn => {
                self.direction = (self.direction.1, self.direction.0);
            }
            Legend::RightTurn => {
                self.direction = (-self.direction.1, -self.direction.0);
            }
            Legend::Space => {
                panic!("Cart went off into space?");
            }
        }
        self.position = go(self.position, self.direction);
    }

    fn legend(&self) -> char {
        match self.direction {
            (0, 1) => '>',
            (-1, 0) => '^',
            (0, -1) => '<',
            (1, 0) => 'v',
            _ => panic!("weird cart direction: {:?}", self.direction),
        }
    }
}

fn parse_map(input: &str) -> (Vec<Cart>, Map) {
    let (width, height) = input
        .lines()
        .fold((0, 0), |acc, line| (max(acc.0, line.len()), acc.1 + 1));

    let mut carts = Vec::new();
    let mut map = Array2::from_elem((height, width), Legend::Space);
    for (row, line) in input.lines().enumerate() {
        for (col, ch) in line.chars().enumerate() {
            let (legend, cart) = Legend::from_char(ch);
            map[[row, col]] = legend;
            if let Some(direction) = cart {
                carts.push(Cart {
                    position: (row, col),
                    direction,
                    next: Choice::Left,
                });
            }
        }
    }

    (carts, map)
}

// carts must be sorted by position
fn print(carts: &Vec<Cart>, map: &Map) {
    let mut next_cart = 0;
    for row in 0..map.len_of(Axis(0)) {
        for col in 0..map.len_of(Axis(1)) {
            if next_cart < carts.len() && (row, col) == carts[next_cart].position {
                print!("{}", carts[next_cart].legend());
                next_cart += 1;
            } else {
                print!("{}", map[[row, col]]);
            }
        }
        println!();
    }
    println!();
}

fn tick(carts: &mut Vec<Cart>, map: &Map) -> Vec<Cart> {
    // Put the carts in the order in which we'll move them.
    carts.sort_by_key(|c| c.position);

    let mut collisions = Vec::new();
    let mut i = 0;
    while i < carts.len() {
        carts[i].step(map);
        // Check for collisions after each cart moves, to prevent
        // face-to-face carts from moving through each other.
        if let Some((j, _)) = carts
            .iter()
            .enumerate()
            .find(|(j, c)| *j != i && c.position == carts[i].position)
        {
            if i < j {
                collisions.extend(carts.drain(j..j + 1));
                collisions.extend(carts.drain(i..i + 1));
            } else {
                collisions.extend(carts.drain(i..i + 1));
                collisions.extend(carts.drain(j..j + 1));
                i -= 1;
            }
        } else {
            i += 1;
        }
    }

    collisions
}

fn main() {
    let (mut carts, map) = parse_map(INPUT);
    //print(&carts, &map);

    for _ in 0.. {
        let collisions = tick(&mut carts, &map);
        if !collisions.is_empty() {
            println!("collisions:");
            for cart in collisions {
                println!("{:?}", cart.position);
            }
        }
        if carts.len() == 1 {
            println!("Final remaining cart: {:?}", carts[0]);
            break;
        }
        //print(&carts, &map);
    }
}

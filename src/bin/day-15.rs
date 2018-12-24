#![allow(dead_code, unused_imports)]

extern crate advent_of_code_2018;
#[macro_use]
extern crate failure;
extern crate ndarray;

use advent_of_code_2018::shortest::shortest_paths;
use failure::Error;
use ndarray::{Array2, Axis};
use std::cmp::max;
use std::fmt;
use std::iter::FromIterator;
use std::str::FromStr;

struct Map(Array2<Square>);

#[derive(Debug, Eq, PartialEq)]
enum Square {
    Empty,
    Wall,
    Unit(Unit)
}

#[derive(Debug, Eq, PartialEq)]
struct Unit {
    tribe: Tribe,
    hit_points: usize
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum Tribe {
    Goblin,
    Elf
}

static INPUT: &str = include_str!("day-15.input");

impl Square {
    fn is_occupyable(&self) -> bool {
        if let Square::Wall = self { false } else { true }
    }

    fn is_unit(&self) -> bool {
        if let Square::Unit(_) = self { true } else { false }
    }
}

impl Unit {
    fn new(tribe: Tribe) -> Unit {
        Unit {
            tribe,
            hit_points: 200,
        }
    }
}

impl Tribe {
    fn is_enemy(&self, other: Tribe) -> bool {
        self != &other // why can't we all just get along
    }
}

impl FromStr for Map {
    type Err = Error;
    fn from_str(s: &str) -> Result<Map, Error> {
        let (width, height) = s.lines()
            .fold((0, 0), |acc, line| (max(acc.0, line.len()), acc.1 + 1));

        let mut map = Array2::from_shape_fn((height, width), |_| Square::Empty);
        for (row, line) in s.lines().enumerate() {
            for (col, ch) in line.chars().enumerate() {
                map[[row, col]] = match ch {
                    '#' => Square::Wall,
                    '.' => Square::Empty,
                    'G' => Square::Unit(Unit::new(Tribe::Goblin)),
                    'E' => Square::Unit(Unit::new(Tribe::Elf)),
                    _ => return Err(format_err!("Bad map character: {:?}", ch)),
                };
            }
        }
        Ok(Map(map))
    }
}

impl Map {
    fn units(&self) -> Vec<(usize, usize)> {
        let mut units = Vec::new();
        for row in 0..self.0.len_of(Axis(0)) {
            for col in 0..self.0.len_of(Axis(1)) {
                match &self.0[[row, col]] {
                    Square::Unit(_) => units.push((row, col)),
                    _ => (),
                }
            }
        }
        units
    }

    fn neighbors(&self, p: (usize, usize)) -> impl Iterator<Item=(usize, usize)> {
        let mut d = (1, 0);
        (0..)
            .map(move |_| {
                d = (-d.1, d.0);
                ((p.0 as isize + d.0) as usize,
                 (p.1 as isize + d.1) as usize)
            })
            .take(4)
    }

    fn occupyable_neighbors<'a>(&'a self, p: (usize, usize)) -> impl Iterator<Item=(usize, usize)> + 'a {
        self.neighbors(p).filter(move |p| self.0[*p].is_occupyable())
    }

    fn closest_enemy_units(&self, start: (usize, usize)) -> Vec<(usize, usize)> {
        let tribe = match &self.0[start] {
            Square::Unit(Unit { tribe, .. }) => *tribe,
            _ => panic!("closest_enemy_units not applied to a unit's square"),
        };

        let mut closest = Vec::new();
        let mut seen = None;
        for (_, to, distance) in shortest_paths(start, |n| self.occupyable_neighbors(*n)) {
            // Don't consider any nodes further than the first enemy we find.
            if let Some(closest) = seen {
                if distance > closest {
                    break;
                }
            }

            match self.0[to] {
                Square::Unit(Unit { tribe: ref other, .. }) if other.is_enemy(tribe) => {
                    closest.push(to);
                    if seen.is_none() {
                        seen = Some(distance);
                    }
                }
                _ => (),
            }
        }

        closest.sort();
        closest.dedup();
        closest
    }
}

fn manhattan(a: (usize, usize), b: (usize, usize)) -> usize {
    fn manhattan1(a: usize, b: usize) -> usize {
        if a >= b { a - b } else { b - a }
    }

    manhattan1(a.0, b.0) + manhattan1(a.1, b.1)
}

#[cfg(test)]
mod test_map {
    use super::*;

    fn trim(pretty: &str) -> String {
        String::from_iter(pretty.lines()
                          .map(str::trim)
                          .filter(|s| !s.is_empty())
                          .flat_map(|s| vec![s, "\n"]))
    }

    #[test]
    fn test_day15_map_fromstr() {
        let map = Map::from_str(&trim("
            #########
            #G......#
            #.E.#...#
            #..##..G#
            #...##..#
            #...#...#
            #.G.E.G.#
            #.....G.#
            #########")).expect("parse test map 1");
        assert_eq!(map.0[[0,0]], Square::Wall);
        assert_eq!(map.0[[1,2]], Square::Empty);
        assert_eq!(map.0[[1,1]], Square::Unit(Unit { tribe: Tribe::Goblin, hit_points: 200 }));
        assert_eq!(map.0[[2,2]], Square::Unit(Unit { tribe: Tribe::Elf, hit_points: 200 }));
        assert_eq!(map.0[[2,4]], Square::Wall);
        assert_eq!(map.units(),
                   vec![(1, 1),
                        (2, 2),
                        (3, 7),
                        (6, 2), (6, 4), (6, 6),
                        (7, 6)]);
        assert_eq!(map.closest_enemy_units((1,1)), vec![(2,2)]);
        assert_eq!(map.closest_enemy_units((3,7)), vec![(6,4)]);
        assert_eq!(map.closest_enemy_units((6,4)), vec![(6,2), (6,6)]);

        let map = Map::from_str(&trim("
            #########
            #.G.....#
            #G.G#...#
            #.G##...#
            #...##..#
            #.G.#...#
            #.......#
            #.......#
            #########")).expect("parse test map 2");
    }
}

impl fmt::Display for Square {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        f.write_str(match self {
            Square::Empty => ".",
            Square::Wall => "#",
            Square::Unit(Unit { tribe: Tribe::Goblin, .. }) => "G",
            Square::Unit(Unit { tribe: Tribe::Elf, .. }) => "E",
        })
    }
}

impl fmt::Display for Map {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        println!();
        for row in 0..self.0.len_of(Axis(0)) {
            for col in 0..self.0.len_of(Axis(1)) {
                self.0[[row, col]].fmt(f)?;
            }
            println!();
        }
        Ok(())
    }
}

#[cfg(test)]
mod test_paths {
    use super::*;

    #[test]
    fn test_adjacents() {
    }
}

fn main() {
}


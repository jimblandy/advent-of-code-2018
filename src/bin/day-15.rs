#![allow(dead_code, unused_imports)]

extern crate advent_of_code_2018 as aoc;
#[macro_use]
extern crate failure;
extern crate ndarray;

use aoc::astar::{astar, Edge};
use aoc::bfs::breadth_first;
use aoc::{first_run, select_iter};
use failure::Error;
use ndarray::{Array2, Axis};
use std::cmp::max;
use std::fmt;
use std::iter::FromIterator;
use std::str::FromStr;

#[derive(Eq, PartialEq)]
struct Map(Array2<Square>);
type Point = (usize, usize);

#[derive(Clone, Debug, Eq, PartialEq)]
enum Square {
    Empty,
    Wall,
    Unit { tribe: Tribe, hit_points: usize },
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum Tribe {
    Goblin,
    Elf,
}

static INPUT: &str = include_str!("day-15.input");

fn manhattan(a: Point, b: Point) -> usize {
    fn manhattan1(a: usize, b: usize) -> usize {
        if a >= b {
            a - b
        } else {
            b - a
        }
    }

    manhattan1(a.0, b.0) + manhattan1(a.1, b.1)
}

impl Square {
    fn new_unit(tribe: Tribe) -> Square {
        Square::Unit {
            tribe,
            hit_points: 200,
        }
    }

    fn is_enemy(&self, tribe: Tribe) -> bool {
        if let Square::Unit { tribe: t, .. } = self {
            tribe.is_enemy(*t)
        } else {
            false
        }
    }
}

impl Tribe {
    fn is_enemy(&self, other: Tribe) -> bool {
        *self != other // why can't we all just get along
    }
}

impl FromStr for Map {
    type Err = Error;
    fn from_str(s: &str) -> Result<Map, Error> {
        let (width, height) = s
            .lines()
            .fold((0, 0), |acc, line| (max(acc.0, line.len()), acc.1 + 1));

        let mut map = Array2::from_shape_fn((height, width), |_| Square::Empty);
        for (row, line) in s.lines().enumerate() {
            for (col, ch) in line.chars().enumerate() {
                map[[row, col]] = match ch {
                    '#' => Square::Wall,
                    '.' => Square::Empty,
                    'G' => Square::new_unit(Tribe::Goblin),
                    'E' => Square::new_unit(Tribe::Elf),
                    _ => return Err(format_err!("Bad map character: {:?}", ch)),
                };
            }
        }
        Ok(Map(map))
    }
}

impl fmt::Debug for Map {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        <Map as fmt::Display>::fmt(self, f)
    }
}

impl Map {
    fn units(&self) -> Vec<Point> {
        let mut units = Vec::new();
        for row in 0..self.0.len_of(Axis(0)) {
            for col in 0..self.0.len_of(Axis(1)) {
                match &self.0[[row, col]] {
                    Square::Unit { .. } => units.push((row, col)),
                    _ => (),
                }
            }
        }
        units
    }

    fn neighbors(&self, p: Point) -> impl Iterator<Item = Point> {
        let mut d = (1, 0);
        (0..)
            .map(move |_| {
                d = (-d.1, d.0);
                ((p.0 as isize + d.0) as usize, (p.1 as isize + d.1) as usize)
            })
            .take(4)
    }

    fn closest_in_range(&self, start: Point) -> Option<Point> {
        // What tribe of unit is moving?
        let tribe = match &self.0[start] {
            Square::Unit { tribe, .. } => *tribe,
            _ => panic!("closest_enemy_units not applied to a unit's square"),
        };

        // Generate all acceptable paths starting from this unit, in
        // breadth-first order.
        let paths = breadth_first(start, |from| {
            // We can move up to, but not through, other units. We represent
            // this by saying that a node with a unit in it has no outgoing
            // edges. However, the unit we're starting from certainly needs to
            // have outgoing edges: it's going to move.
            select_iter(
                self.0[*from] == Square::Empty || *from == start,
                self.neighbors(*from)
                    .filter(|to| self.0[*to] != Square::Wall),
                std::iter::empty(),
            )
        });

        // Limit the traversal to edges arriving at the closest enemies.
        let closest = first_run(paths, |edge: &(Point, Point, usize)| {
            if self.0[edge.1].is_enemy(tribe) {
                // Once we've reached one enemy, ignore any
                // enemies that are further away.
                let closest = edge.2;
                Some(move |edge: &(Point, Point, usize)| edge.2 <= closest)
            } else {
                None
            }
        })
        .filter(|&(_, to, _)| self.0[to].is_enemy(tribe));

        // All we actually care about are the 'in range' squares from which we
        // can attack some enemy.
        let mut in_range = closest.map(|(from, _, _)| from).collect::<Vec<_>>();

        // Among all 'in range' squares, choose the one that comes first in
        // reading order.
        in_range.sort();
        in_range.get(0).cloned()
    }

    fn step_towards(&self, unit: Point, chosen: Point) -> Point {
        eprintln!("        steps towards {:?}", chosen);

        // Find all shortest paths going backwards from the chosen square to the
        // unit, and then take the final edges' origin as a candidate square for
        // the unit to move into.
        let paths_back = astar(chosen, |from| {
            self.neighbors(*from)
                .filter(|to| self.0[*to] == Square::Empty || *to == unit)
                .map(|n| (n, manhattan(n, unit)))
        });

        let paths_back = paths_back.inspect(|e| {
            eprintln!("        unculled step_towards edge: {:?}", e);
        });

        // Limit the traversal to edges along the shortest paths arriving at the unit.
        let first_moves = first_run(paths_back, |edge: &Edge<Point>| {
            if edge.to == unit {
                let closest = edge.path_length;
                Some(move |edge: &Edge<Point>| edge.path_length <= closest)
            } else {
                None
            }
        })
        .filter(|edge| edge.to == unit);

        // Extract those edge's starting positions.
        let mut first_moves = first_moves.map(|e| e.from).collect::<Vec<_>>();
        first_moves.sort();
        eprintln!("        possible first moves: {:?}", first_moves);
        first_moves[0]
    }

    fn turn(&mut self, unit: Point) -> bool {
        eprintln!("    turn for {:?}", unit);
        if let Some(in_range) = self.closest_in_range(unit) {
            if unit == in_range {
                eprintln!("        in range, but attack isn't implemented yet");
            } else {
                let move_to = self.step_towards(unit, in_range);
                assert!(self.0[move_to] == Square::Empty);
                self.0.swap(unit, move_to);
            }
            true
        } else {
            false
        }
    }

    fn round(&mut self) -> bool {
        eprintln!("round");
        let mut any = false;
        for unit in self.units() {
            if self.turn(unit) {
                any = true;
            }
        }
        any
    }
}

#[cfg(test)]
mod test_map {
    use super::*;

    fn trim(pretty: &str) -> String {
        String::from_iter(
            pretty
                .lines()
                .map(str::trim)
                .filter(|s| !s.is_empty())
                .flat_map(|s| vec![s, "\n"]),
        )
    }

    fn test_map(pretty: &str) -> Map {
        Map::from_str(&trim(pretty)).expect("parse test map")
    }

    #[test]
    fn test_day15_map() {
        let map = test_map("
            ####
            #GE#
            #..#
            ####");
        assert_eq!(map.closest_in_range((1,1)), Some((1,1)));

        let mut map = test_map("
            #########
            #G......#
            #.E.#...#
            #..##..G#
            #...##..#
            #...#...#
            #.G.E.G.#
            #.....G.#
            #########");
        assert_eq!(map.0[[0,0]], Square::Wall);
        assert_eq!(map.0[[1,2]], Square::Empty);
        assert_eq!(map.0[[1,1]], Square::Unit { tribe: Tribe::Goblin, hit_points: 200 });
        assert_eq!(map.0[[2,2]], Square::Unit { tribe: Tribe::Elf, hit_points: 200 });
        assert_eq!(map.0[[2,4]], Square::Wall);
        assert_eq!(map.units(),
                   vec![(1, 1),
                        (2, 2),
                        (3, 7),
                        (6, 2), (6, 4), (6, 6),
                        (7, 6)]);
        assert_eq!(map.closest_in_range((1,1)), Some((1,2)));
        assert_eq!(map.step_towards((1,1), (1,2)), (1,2));

        assert_eq!(map.closest_in_range((2,2)), Some((1,2)));
        assert_eq!(map.step_towards((2,2), (1,1)), (1,2));

        assert_eq!(map.closest_in_range((3,7)), Some((6,5)));
        assert_eq!(map.step_towards((3,7), (6,4)), (3,6));

        assert_eq!(map.closest_in_range((6,2)), Some((6,3)));
        assert_eq!(map.step_towards((6,2), (6,4)), (6,3));

        assert_eq!(map.closest_in_range((6,4)), Some((6,3)));
        assert_eq!(map.step_towards((6,4), (6,2)), (6,3));
        assert_eq!(map.step_towards((6,4), (6,6)), (6,5));

        assert_eq!(map.closest_in_range((6,6)), Some((6,5)));
        assert_eq!(map.step_towards((6,6), (6,4)), (6,5));

        assert_eq!(map.closest_in_range((7,6)), Some((6,5)));
        assert_eq!(map.step_towards((7,6), (6,4)), (7,5));

        assert_eq!(map.round(), true);
        assert_eq!(map, test_map("
            #########
            #.G.....#
            #.E.#...#
            #..##.G.#
            #...##..#
            #...#...#
            #..GEG..#
            #....G..#
            #########"));

        let map = test_map("
            #######
            #E..G.#
            #...#.#
            #.G.#G#
            #######");
        assert_eq!(map.closest_in_range((1,1)), Some((1,3)));
        assert_eq!(map.step_towards((1,1), (1,4)), (1,2));
        assert_eq!(map.closest_in_range((1,4)), Some((1,2)));
        assert_eq!(map.step_towards((1,4), (1,1)), (1,3));
        assert_eq!(map.closest_in_range((3,2)), Some((1,2)));
        assert_eq!(map.step_towards((3,2), (1,1)), (2,2));
        assert_eq!(map.closest_in_range((3,5)), None);

        let mut map = test_map("
            #######
            #.E...#
            #.....#
            #...G.#
            #######");
        assert!(map.round());
        assert_eq!(map, test_map("
            #######
            #..E..#
            #...G.#
            #.....#
            #######"));

        eprintln!("big map");
        let mut map = test_map("
            #########
            #G..G..G#
            #.......#
            #.......#
            #G..E..G#
            #.......#
            #.......#
            #G..G..G#
            #########");
        assert!(map.round());
        assert_eq!(map, test_map("
            #########
            #.G...G.#
            #...G...#
            #...E..G#
            #.G.....#
            #.......#
            #G..G..G#
            #.......#
            #########"));
        assert!(map.round());
        assert_eq!(map, test_map("
            #########
            #..G.G..#
            #...G...#
            #.G.E.G.#
            #.......#
            #G..G..G#
            #.......#
            #.......#
            #########"));
        assert!(map.round());
        assert_eq!(map, test_map("
            #########
            #.......#
            #..GGG..#
            #..GEG..#
            #G..G...#
            #......G#
            #.......#
            #.......#
            #########"));

        let mut map = test_map("
            ################################
            #################..#############
            ##########..###G..##############
            #######...G..#G..#...###..######
            #######....###......###...######
            #####G..G..###........#...G..###
            #####G......##.....G....G....###
            ###....G....###..............#.#
            ###.G........#.G...............#
            ###............G............#..#
            ###G.....G##..................##
            ###.......#.E...G...........####
            ##..........G.#####............#
            #####........#######...E...E...#
            #####.E.....#########.....#.#.##
            ##.#...G....#########.###.#.#.##
            #...........#########.##########
            ###......G..#########.##########
            ##..........#########.##########
            #....##.G....#######.....#######
            ###E.##....E..#####..E....######
            #######.#................#######
            ########......#E.....###########
            #########......E.....###########
            ##########.........E############
            ##########.........#############
            ##########..###..###############
            ##########..###..###############
            ###########..###...#############
            ##########...#####....##########
            ###########..########...########
            ################################");
        assert_eq!(map.closest_in_range((3,10)), Some((10,12)));
        assert_eq!(map.closest_in_range((11,12)), Some((11,12)));
        assert_eq!(map.closest_in_range((12,12)), Some((12,12)));
        assert_eq!(map.closest_in_range((23,15)), Some((19,9)));
    }
}

impl fmt::Display for Square {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        f.write_str(match self {
            Square::Empty => ".",
            Square::Wall => "#",
            Square::Unit { tribe: Tribe::Goblin, .. } => "G",
            Square::Unit { tribe: Tribe::Elf, .. } => "E",
        })
    }
}

impl fmt::Display for Map {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        f.write_str("\n")?;
        for row in 0..self.0.len_of(Axis(0)) {
            for col in 0..self.0.len_of(Axis(1)) {
                self.0[[row, col]].fmt(f)?;
            }
            f.write_str("\n")?;
        }
        Ok(())
    }
}

fn main() {}

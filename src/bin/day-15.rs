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

    fn symbol(&self) -> char {
        match self {
            Tribe::Elf => 'E',
            Tribe::Goblin => 'G',
        }
    }
}

impl FromStr for Map {
    type Err = Error;
    fn from_str(s: &str) -> Result<Map, Error> {
        fn map_and_hp(s: &str) -> (&str, &str) {
            let s = s.trim();
            // Does this line have HP information?
            if let Some(map_end) = s.find(' ') {
                (&s[..map_end], s[map_end..].trim())
            } else {
                (s, "")
            }
        }

        let (width, height) = s
            .lines()
            .fold((0, 0), |acc, line| (max(acc.0, map_and_hp(line).0.len()), acc.1 + 1));

        let mut map = Array2::from_shape_fn((height, width), |_| Square::Empty);
        for (row, line) in s.lines().enumerate() {
            let (line_map, hps) = map_and_hp(line);
            for (col, ch) in line_map.chars().enumerate() {
                map[[row, col]] = match ch {
                    '#' => Square::Wall,
                    '.' => Square::Empty,
                    'G' => Square::new_unit(Tribe::Goblin),
                    'E' => Square::new_unit(Tribe::Elf),
                    _ => return Err(format_err!("Bad map character: {:?}", ch)),
                };
            }
            let mut units_hp_set = 0;
            if !hps.is_empty() {
                for hp in hps.split(',').map(str::trim) {
                    let mut cursor = hp.chars();
                    let next_tribe = match cursor.next() {
                        Some('G') => Tribe::Goblin,
                        Some('E') => Tribe::Elf,
                        ch => return Err(format_err!("Bad hp tribe character: {:?}", ch)),
                    };
                    if cursor.next() != Some('(') {
                        return Err(format_err!("expected '(' after hp tribe"));
                    }
                    let tail = cursor.as_str();
                    let num_end = if let Some(num_end) = tail.find(')') {
                        num_end
                    } else {
                        return Err(format_err!("expected ')' after hp"));
                    };
                    loop {
                        units_hp_set += 1;
                        if units_hp_set > width {
                            return Err(format_err!("hp data has more units that map"));
                        }
                        match &mut map[[row, units_hp_set - 1]] {
                            Square::Unit { tribe, hit_points } => {
                                if *tribe != next_tribe {
                                    return Err(format_err!("hp data doesn't match units in map"));
                                }
                                *hit_points = usize::from_str(&tail[..num_end])?;
                                break;
                            }
                            _ => (),
                        }
                    }
                }
            }
        }
        Ok(Map(map))
    }
}

impl Map {
    /// Run combat to completion. Return the number of full rounds completed,
    /// and the total number of hit points possesed by the surviving units.
    ///
    /// The directions say that combat ends only when there are no
    /// 'targets (enemy units)' left, but if there are enemies that can't
    /// reach each other, it doesn't make any sense to continue combat.
    fn combat(&mut self) -> (usize, usize) {
        let mut rounds = 0;
        loop {
            //eprintln!("Round #{} begins:", rounds + 1);
            if !self.round() {
                break;
            }
            rounds += 1;
        }
        (rounds, self.total_remaining_hit_points())
    }

    /// Run a round of combat. Return false if any unit had no enemies when its
    /// turn arrived, true otherwise.
    fn round(&mut self) -> bool {
        //eprintln!("round");
        for unit in self.units() {
            // The unit might have been killed after we gathered the list of units.
            // Iterator invalidation!
            if let Square::Unit { .. } = self.0[unit] {
                if !self.turn(unit) {
                    return false;
                }
            }
        }
        true
    }

    /// Let the unit at `unit` take a turn. Return true if any enemies existed
    /// at all, regardless of whether they were reachable or the unit was able
    /// to do anything about them.
    fn turn(&mut self, mut unit: Point) -> bool {
        //eprintln!("    turn for {:?}", unit);

        // First, choose a target.
        let in_range = match self.closest_in_range(unit) {
            Ok(in_range) => in_range,
            Err(any_at_all) => return any_at_all,
        };

        // Then, move.
        if unit != in_range {
            let move_to = self.step_towards(unit, in_range);
            assert!(self.0[move_to] == Square::Empty);
            self.0.swap(unit, move_to);
            unit = move_to;
        }

        // Finally, attack if in range.
        if unit == in_range {
            self.attack(unit);
        }

        true
    }

    /// If any enemies are in range for `unit`, return the closest, earliest unit.
    /// On error, return whether there are any enemy units present at all.
    fn closest_in_range(&self, start: Point) -> Result<Point, bool> {
        // What tribe of unit is moving?
        let tribe = self.tribe_at(start);

        // Generate all acceptable paths starting from this unit, in
        // breadth-first order.
        let paths = breadth_first(start, |from| {
            // We can move up to, but not through, other units. We represent
            // this by saying that a node with a unit in it has no outgoing
            // edges. However, the unit we're starting from certainly needs to
            // have outgoing edges: it's the one that's going to move.
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
        match in_range.get(0) {
            Some(first) => Ok(*first),
            None => {
                Err(self.units().into_iter().any(|u| match self.0[u] {
                    Square::Unit { tribe: t, .. } => t.is_enemy(tribe),
                    _ => false,
                }))
            }
        }
    }

    fn step_towards(&self, unit: Point, chosen: Point) -> Point {
        //eprintln!("        steps towards {:?}", chosen);

        // Find all shortest paths going backwards from the chosen square to the
        // unit, and then take the final edges' origin as a candidate square for
        // the unit to move into.
        let paths_back = astar(chosen, |from| {
            self.neighbors(*from)
                .filter(|to| self.0[*to] == Square::Empty || *to == unit)
                .map(|n| (n, manhattan(n, unit)))
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
        //eprintln!("        possible first moves: {:?}", first_moves);
        first_moves[0]
    }

    fn attack(&mut self, unit: Point) {
        let my_tribe = self.tribe_at(unit);

        let weakest_enemy = {
            let adjacent_enemies = self.neighbors(unit)
                .filter_map(|n| match self.0[n] {
                    Square::Unit { tribe, hit_points } if my_tribe.is_enemy(tribe) =>
                        Some((n, hit_points)),
                    _ => None,
                });

            // What is the strength of the weakest enemy?
            let weakest_strength = adjacent_enemies
                .clone()
                .map(|(_, hit_points)| hit_points)
                .min()
                .expect("shouldn't have attacked unless we were in range of somebody");

            // Among the enemies who are weakest, which falls first in reading
            // order?
            let mut weakest_enemies = adjacent_enemies
                .filter(|(_pos, hit_points)| *hit_points == weakest_strength)
                .map(|(pos, _hit_points)| pos)
                .collect::<Vec<_>>();
            weakest_enemies.sort();
            weakest_enemies[0]
        };

        // Attack!
        if match &mut self.0[weakest_enemy] {
            Square::Unit { hit_points, .. } => {
                *hit_points = hit_points.saturating_sub(3);
                //eprintln!("        attacks {:?}, hp now {}", weakest_enemy, *hit_points);
                *hit_points == 0
            },
            _ => panic!("should have been an enemy"),
        } {
            // Defeated!
            self.0[weakest_enemy] = Square::Empty;
        };
    }

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

    fn neighbors(&self, p: Point) -> impl Iterator<Item = Point> + Clone {
        let mut d = (1, 0);
        (0..)
            .map(move |_| {
                d = (-d.1, d.0);
                ((p.0 as isize + d.0) as usize, (p.1 as isize + d.1) as usize)
            })
            .take(4)
    }

    fn tribe_at(&self, p: Point) -> Tribe {
        match self.0[p] {
            Square::Unit { tribe, .. } => tribe,
            _ => panic!("tribe_at: square does not hold a unit"),
        }
    }

    fn total_remaining_hit_points(&self) -> usize {
        self.units()
            .into_iter()
            .filter_map(|p| match self.0[p] {
                Square::Unit { hit_points, .. } => Some(hit_points),
                _ => None
            })
            .sum()
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
            let mut sep = " ";
            for col in 0..self.0.len_of(Axis(1)) {
                match self.0[[row,col]] {
                    Square::Unit { tribe, hit_points } => {
                        write!(f, "{}{}({})", sep, tribe.symbol(), hit_points)?;
                        sep = ", ";
                    }
                    _ => (),
                }
            }
            f.write_str("\n")?;
        }
        Ok(())
    }
}

impl fmt::Debug for Map {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        <Map as fmt::Display>::fmt(self, f)
    }
}

#[cfg(test)]
mod test_map {
    use super::*;
    use std::iter::FromIterator;

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
        assert_eq!(map.closest_in_range((1,1)), Ok((1,1)));

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
        assert_eq!(map.closest_in_range((1,1)), Ok((1,2)));
        assert_eq!(map.step_towards((1,1), (1,2)), (1,2));

        assert_eq!(map.closest_in_range((2,2)), Ok((1,2)));
        assert_eq!(map.step_towards((2,2), (1,1)), (1,2));

        assert_eq!(map.closest_in_range((3,7)), Ok((6,5)));
        assert_eq!(map.step_towards((3,7), (6,4)), (3,6));

        assert_eq!(map.closest_in_range((6,2)), Ok((6,3)));
        assert_eq!(map.step_towards((6,2), (6,4)), (6,3));

        assert_eq!(map.closest_in_range((6,4)), Ok((6,3)));
        assert_eq!(map.step_towards((6,4), (6,2)), (6,3));
        assert_eq!(map.step_towards((6,4), (6,6)), (6,5));

        assert_eq!(map.closest_in_range((6,6)), Ok((6,5)));
        assert_eq!(map.step_towards((6,6), (6,4)), (6,5));

        assert_eq!(map.closest_in_range((7,6)), Ok((6,5)));
        assert_eq!(map.step_towards((7,6), (6,4)), (7,5));

        assert_eq!(map.round(), true);
        assert_eq!(map, test_map("
            #########
            #.G.....# G(197)
            #.E.#...# E(197)
            #..##.G.#
            #...##..#
            #...#...#
            #..GEG..# G(197), E(194), G(200)
            #....G..# G(200)
            #########"));

        let map = test_map("
            #######
            #E..G.#
            #...#.#
            #.G.#G#
            #######");
        assert_eq!(map.closest_in_range((1,1)), Ok((1,3)));
        assert_eq!(map.step_towards((1,1), (1,4)), (1,2));
        assert_eq!(map.closest_in_range((1,4)), Ok((1,2)));
        assert_eq!(map.step_towards((1,4), (1,1)), (1,3));
        assert_eq!(map.closest_in_range((3,2)), Ok((1,2)));
        assert_eq!(map.step_towards((3,2), (1,1)), (2,2));
        assert_eq!(map.closest_in_range((3,5)), Err(true));

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
            #...G...# G(197)
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
            #...G...# G(194)
            #.G.E.G.# G(200), E(197), G(200)
            #.......#
            #G..G..G#
            #.......#
            #.......#
            #########"));
        assert!(map.round());
        assert_eq!(map, test_map("
            #########
            #.......#
            #..GGG..# G(200), G(191), G(200)
            #..GEG..# G(200), E(185), G(200)
            #G..G...#
            #......G#
            #.......#
            #.......#
            #########"));

        let map = test_map("
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
        assert_eq!(map.closest_in_range((3,10)), Ok((10,12)));
        assert_eq!(map.closest_in_range((11,12)), Ok((11,12)));
        assert_eq!(map.closest_in_range((12,12)), Ok((12,12)));
        assert_eq!(map.closest_in_range((23,15)), Ok((19,9)));

        let mut map = test_map("
            G....
            ..G..
            ..EG.
            ..G..
            ...G.");
        assert_eq!(map.0[(2,3)], Square::Unit { tribe: Tribe::Goblin, hit_points: 200 });
        map.0[(2,3)] = Square::Unit { tribe: Tribe::Goblin, hit_points: 2 };
        map.attack((2,2));
        assert_eq!(map.0[(2,3)], Square::Empty);

        let mut map = test_map("
            #######
            #.G...#   G(200)
            #...EG#   E(200), G(200)
            #.#.#G#   G(200)
            #..G#E#   G(200), E(200)
            #.....#
            #######");


        // After 1 round:
        assert!(map.round());
        assert_eq!(map, test_map("
            #######
            #..G..#   G(200)
            #...EG#   E(197), G(197)
            #.#G#G#   G(200), G(197)
            #...#E#   E(197)
            #.....#
            #######"));

        // After 2 rounds:
        assert!(map.round());
        assert_eq!(map, test_map("
            #######
            #...G.#   G(200)
            #..GEG#   G(200), E(188), G(194)
            #.#.#G#   G(194)
            #...#E#   E(194)
            #.....#
            #######"));

        // Combat ensues; eventually, the top Elf dies:
        // After 23 rounds:
        for _ in 2..23 {
            assert!(map.round());
        }
        assert_eq!(map, test_map("
            #######
            #...G.#   G(200)
            #..G.G#   G(200), G(131)
            #.#.#G#   G(131)
            #...#E#   E(131)
            #.....#
            #######"));

        // After 24 rounds:
        assert!(map.round());
        assert_eq!(map, test_map("
            #######
            #..G..#   G(200)
            #...G.#   G(131)
            #.#G#G#   G(200), G(128)
            #...#E#   E(128)
            #.....#
            #######"));

        // After 25 rounds:
        assert!(map.round());
        assert_eq!(map, test_map("
            #######
            #.G...#   G(200)
            #..G..#   G(131)
            #.#.#G#   G(125)
            #..G#E#   G(200), E(125)
            #.....#
            #######"));

        // After 26 rounds:
        assert!(map.round());
        assert_eq!(map, test_map("
            #######
            #G....#   G(200)
            #.G...#   G(131)
            #.#.#G#   G(122)
            #...#E#   E(122)
            #..G..#   G(200)
            #######"));

        // After 27 rounds:
        assert!(map.round());
        assert_eq!(map, test_map("
            #######
            #G....#   G(200)
            #.G...#   G(131)
            #.#.#G#   G(119)
            #...#E#   E(119)
            #...G.#   G(200)
            #######"));

        // After 28 rounds:
        assert!(map.round());
        assert_eq!(map, test_map("
            #######
            #G....#   G(200)
            #.G...#   G(131)
            #.#.#G#   G(116)
            #...#E#   E(113)
            #....G#   G(200)
            #######"));

        // More combat ensues; eventually, the bottom Elf dies:
        // After 47 rounds:
        for _ in 28..47 {
            assert!(map.round());
        }
        assert_eq!(map, test_map("
            #######
            #G....#   G(200)
            #.G...#   G(131)
            #.#.#G#   G(59)
            #...#.#
            #....G#   G(200)
            #######"));

        // Same battle, just counting rounds:
        let mut map = test_map("
            #######
            #.G...#   G(200)
            #...EG#   E(200), G(200)
            #.#.#G#   G(200)
            #..G#E#   G(200), E(200)
            #.....#
            #######");
        assert_eq!(map.combat(), (47, 590));

        let mut map = test_map("
            #######
            #G..#E#
            #E#E.E#
            #G.##.#
            #...#E#
            #...E.#
            #######");
        assert_eq!(map.combat(), (37, 982));
        assert_eq!(map, test_map("
            #######
            #...#E#   E(200)
            #E#...#   E(197)
            #.E##.#   E(185)
            #E..#E#   E(200), E(200)
            #.....#
            #######"));

        let mut map = test_map("
            #######
            #E..EG#
            #.#G.E#
            #E.##E#
            #G..#.#
            #..E#.#
            #######");
        assert_eq!(map.combat(), (46, 859));
        assert_eq!(map, test_map("
            #######
            #.E.E.#   E(164), E(197)
            #.#E..#   E(200)
            #E.##.#   E(98)
            #.E.#.#   E(200)
            #...#.#
            #######   "));

        let mut map = test_map("
            #######
            #E.G#.#
            #.#G..#
            #G.#.G#
            #G..#.#
            #...E.#
            #######");
        assert_eq!(map.combat(), (35, 793));
        assert_eq!(map, test_map("
            #######
            #G.G#.#   G(200), G(98)
            #.#G..#   G(200)
            #..#..#
            #...#G#   G(95)
            #...G.#   G(200)
            #######   "));

        let mut map = test_map("
            #######
            #.E...#
            #.#..G#
            #.###.#
            #E#G#G#
            #...#G#
            #######");
        assert_eq!(map.combat(), (54, 536));
        assert_eq!(map, test_map("
            #######
            #.....#
            #.#G..#   G(200)
            #.###.#
            #.#.#.#
            #G.G#G#   G(98), G(38), G(200)
            #######   "));

        let mut map = test_map("
            #########
            #G......#
            #.E.#...#
            #..##..G#
            #...##..#
            #...#...#
            #.G...G.#
            #.....G.#
            #########");
        assert_eq!(map.combat(), (20, 937));
        assert_eq!(map, test_map("
            #########
            #.G.....#   G(137)
            #G.G#...#   G(200), G(200)
            #.G##...#   G(200)
            #...##..#
            #.G.#...#   G(200)
            #.......#
            #.......#
            #########"));
    }
}

fn main() -> Result<(), Error> {
    let mut map = Map::from_str(INPUT)?;
    println!("Initial map:{}", map);
    let (rounds, total_hp) = map.combat();
    println!("Combat ends after {} full rounds, with {} total hit points left",
             rounds, total_hp);
    println!("Outcome: {} * {} = {}", rounds, total_hp, rounds * total_hp);
    println!("Final map:{}", map);
    Ok(())
}

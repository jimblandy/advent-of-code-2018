#![feature(iter_unfold)]

extern crate advent_of_code_2018 as aoc;
extern crate ndarray;

use aoc::astar_weighted::astar_weighted;
use aoc::Manhattan;
use ndarray::{Array2, Axis};
use std::collections::HashMap;
use std::fmt;
use std::iter::{from_fn, FromIterator};

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
struct GeologicIndex(usize);

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
struct ErosionLevel(usize);

type Map = Array2<GeologicIndex>;
type Point = (usize, usize);

#[derive(Clone, Debug, Eq, PartialEq)]
enum Region {
    Rocky,
    Wet,
    Narrow,
}

const DEPTH: usize = 3066;
const TARGET: Point = (13, 726);
const MODULUS: usize = 20183;

fn geologic_index(size: Point, depth: usize, target: Point) -> Map {
    let mut index = Map::from_elem((size.0 + 1, size.1 + 1), GeologicIndex(0));
    for x in 0..=size.0 {
        index[[x, 0]] = GeologicIndex((x * 16807) % MODULUS);
    }
    for y in 0..=size.1 {
        index[[0, y]] = GeologicIndex((y * 48271) % MODULUS);
    }
    for x in 1..=size.0 {
        for y in 1..=size.1 {
            index[[x, y]] = GeologicIndex(
                (erosion_level(index[[x - 1, y]], depth).0
                    * erosion_level(index[[x, y - 1]], depth).0)
                    % MODULUS,
            );
        }
    }
    index[target] = GeologicIndex(0);
    index
}

fn erosion_level(gi: GeologicIndex, depth: usize) -> ErosionLevel {
    ErosionLevel((gi.0 + depth) % MODULUS)
}

fn risk_level(gi: &Map, depth: usize, target: Point) -> usize {
    let mut r = 0;
    for x in 0..=target.0 {
        for y in 0..=target.1 {
            r += Region::from(erosion_level(gi[[x, y]], depth)).risk_level()
        }
    }
    r
}

#[test]
fn test_erosion_level() {
    let gi = geologic_index((10, 10), 510, (10, 10));
    assert_eq!(erosion_level(gi[(0, 0)], 510), ErosionLevel(510));
    assert_eq!(erosion_level(gi[(1, 0)], 510), ErosionLevel(17317));
    assert_eq!(erosion_level(gi[(0, 1)], 510), ErosionLevel(8415));
    assert_eq!(gi[[1, 1]], GeologicIndex(145722555 % MODULUS));
    assert_eq!(erosion_level(gi[(1, 1)], 510), ErosionLevel(1805));
    assert_eq!(erosion_level(gi[(10, 10)], 510), ErosionLevel(510));
    assert_eq!(risk_level(&gi, 510, (10, 10)), 114);
}

impl From<ErosionLevel> for Region {
    fn from(erosion_level: ErosionLevel) -> Region {
        match erosion_level.0 % 3 {
            0 => Region::Rocky,
            1 => Region::Wet,
            2 => Region::Narrow,
            _ => panic!("modulo broken"),
        }
    }
}

impl Region {
    fn all_compatible(&self) -> [Equipage; 2] {
        match self {
            Region::Rocky => [Equipage::Climb, Equipage::Torch],
            Region::Wet => [Equipage::Climb, Equipage::Neith],
            Region::Narrow => [Equipage::Torch, Equipage::Neith],
        }
    }

    fn is_compatible(&self, equip: Equipage) -> bool {
        let c = self.all_compatible();
        equip == c[0] || equip == c[1]
    }

    fn risk_level(&self) -> usize {
        match self {
            Region::Rocky => 0,
            Region::Wet => 1,
            Region::Narrow => 2,
        }
    }
}

impl fmt::Display for Region {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(
            f,
            "{}",
            match self {
                Region::Rocky => '.',
                Region::Wet => '=',
                Region::Narrow => '|',
            }
        )
    }
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
enum Equipage {
    Neith,
    Climb,
    Torch,
}

/// The nodes in our weighted depth-first search of the cave are not just
/// positions, but also states of equipage. The cost of moving from one square
/// to another depends on whether an equipment change is needed, and in some
/// cases an equipment change is *optional*, so we need to consider the cases
/// where we did and did not change equipment *in the past* as separate states
/// to move forward from.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
struct RescueState {
    position: Point,
    equipped: Equipage,
}

impl RescueState {
    fn neighbors(&self, gi: &Map, depth: usize, goal: Point) -> Vec<(RescueState, usize, usize)> {
        // Compute adjacent states that are moves.
        let mut state = (1, 0);
        let adjacents = from_fn(|| {
            state = (-state.1, state.0);
            Some(state)
        })
        .take(4)
        .filter_map(|(dx, dy)| {
            let (x, y) = self.position;
            if x as isize + dx < 0 || y as isize + dy < 0 {
                return None;
            }
            if x as isize + dx >= gi.len_of(Axis(0)) as isize
                || y as isize + dy >= gi.len_of(Axis(1)) as isize
            {
                panic!(
                    "Considered regions beyond limits of map: neighbor of {:?}",
                    (x, y)
                );
            }
            Some(((x as isize + dx) as usize, (y as isize + dy) as usize))
        })
        .filter(|&pos| Region::from(erosion_level(gi[pos], depth)).is_compatible(self.equipped))
        .map(|position| {
            (
                RescueState { position, ..*self },
                1,
                position.manhattan(goal),
            )
        });

        let mut neighbors = Vec::from_iter(adjacents);

        // Add adjacent states that are equipment changes.
        for &equipped in &Region::from(erosion_level(gi[self.position], depth)).all_compatible() {
            if equipped != self.equipped {
                neighbors.push((
                    RescueState { equipped, ..*self },
                    7,
                    self.position.manhattan(goal),
                ));
            }
        }

        neighbors
    }
}

#[rustfmt::skip]
#[test]
fn test_rescue_state() {
    use ndarray::arr2;

    let map = arr2(&[
        [0, 1, 2], // rocky, wet, narrow
        [2, 2, 0], // narrow, narrow, rocky
        [1, 1, 0], // wet, wet, goal (rocky)
        [0, 0, 0],
    ]);
    let map = map.map(|&i| GeologicIndex(i));
    let state = RescueState { position: (0,0), equipped: Equipage::Torch };
    assert_eq!(state.neighbors(&map, 0, (2,2)),
               vec![(RescueState { position: (1,0), equipped: Equipage::Torch }, 1, 3),
                    (RescueState { position: (0,0), equipped: Equipage::Climb }, 7, 4)]);

    let state = RescueState { position: (0,0), equipped: Equipage::Climb };
    assert_eq!(state.neighbors(&map, 0, (2,2)),
               vec![(RescueState { position: (0,1), equipped: Equipage::Climb }, 1, 3),
                    (RescueState { position: (0,0), equipped: Equipage::Torch }, 7, 4)]);

    let state = RescueState { position: (0,1), equipped: Equipage::Neith };
    assert_eq!(state.neighbors(&map, 0, (2,2)),
               vec![(RescueState { position: (0,2), equipped: Equipage::Neith }, 1, 2),
                    (RescueState { position: (1,1), equipped: Equipage::Neith }, 1, 2),
                    (RescueState { position: (0,1), equipped: Equipage::Climb }, 7, 3)]);

    let state = RescueState { position: (0,1), equipped: Equipage::Climb };
    assert_eq!(state.neighbors(&map, 0, (2,2)),
               vec![(RescueState { position: (0,0), equipped: Equipage::Climb }, 1, 4),
                    (RescueState { position: (0,1), equipped: Equipage::Neith }, 7, 3)]);

    let state = RescueState { position: (2,1), equipped: Equipage::Climb };
    assert_eq!(state.neighbors(&map, 0, (2,2)),
               vec![(RescueState { position: (2,2), equipped: Equipage::Climb }, 1, 0),
                    (RescueState { position: (2,0), equipped: Equipage::Climb }, 1, 2),
                    (RescueState { position: (3,1), equipped: Equipage::Climb }, 1, 2),
                    (RescueState { position: (2,1), equipped: Equipage::Neith }, 7, 1)]);

    let state = RescueState { position: (2,1), equipped: Equipage::Neith };
    assert_eq!(state.neighbors(&map, 0, (2,2)),
               vec![(RescueState { position: (1,1), equipped: Equipage::Neith }, 1, 2),
                    (RescueState { position: (2,0), equipped: Equipage::Neith }, 1, 2),
                    (RescueState { position: (2,1), equipped: Equipage::Climb }, 7, 1)]);
}

fn print_fastest_path(gi: &Map, depth: usize, target: Point) {
    println!("Risk level: {}", risk_level(&gi, depth, target));

    let mut back = HashMap::new();
    let ending_state = astar_weighted(
        RescueState {
            position: (0, 0),
            equipped: Equipage::Torch,
        },
        |state| state.neighbors(&gi, depth, target).into_iter(),
    )
    .inspect(|edge| {
        //eprintln!("Considering: {:?}", edge);
        back.entry(edge.to.clone()).or_insert(edge.from.clone());
    })
    .filter(|edge| edge.to.position == target && edge.to.equipped == Equipage::Torch)
    .next()
    .unwrap();

    println!("Ending state: {:?}", ending_state);
    println!("Backtrace:");
    let mut state = ending_state.to.clone();
    loop {
        println!(
            "    {:?}, ({})",
            state,
            Region::from(erosion_level(gi[state.position], depth))
        );
        if state.position == (0, 0) {
            break;
        }
        if let Some(prev) = back.get(&state) {
            state = prev.clone();
        } else {
            break;
        }
    }
}

fn main() {
    let gi = geologic_index((30, 30), 510, (10, 10));
    for y in 0..=15 {
        for x in 0..=15 {
            print!("{}", Region::from(erosion_level(gi[[x, y]], 510)));
        }
        println!();
    }
    print_fastest_path(&gi, 510, (10, 10));

    let gi = geologic_index((400, 800), DEPTH, TARGET);
    print_fastest_path(&gi, DEPTH, TARGET);
}

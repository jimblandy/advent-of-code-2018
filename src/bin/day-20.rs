#![feature(try_from)]

extern crate advent_of_code_2018 as aoc;
#[macro_use]
extern crate failure;
extern crate ndarray;

use aoc::bfs::breadth_first;
use aoc::{cartesian_product, union_ranges, Cursor};
use failure::Error;
use ndarray::{Array2, ArrayView1};
use std::collections::HashSet;
use std::convert::{TryFrom, TryInto};
use std::fmt;
use std::iter::FromIterator;
use std::ops::Range;
use std::str::FromStr;

#[allow(dead_code)]
static INPUT: &str = include_str!("day-20.input");

#[derive(Debug, Eq, PartialEq)]
struct Concat {
    head: Vec<(Run, Alt)>,
    tail: Run,
}

#[derive(Debug, Eq, PartialEq)]
struct Alt {
    alternatives: Vec<Concat>,
}

type Run = Vec<Dir>;

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum Dir {
    North,
    South,
    East,
    West,
}

#[derive(Clone, Copy, Debug, Default, Eq, PartialEq)]
struct DirSet(u8);

struct Map(Array2<DirSet>);

type Point = (usize, usize);
type PointSet = HashSet<Point>;
type Delta = (isize, isize);
type DeltaSet = HashSet<Delta>;
type DeltaBounds = (Range<isize>, Range<isize>);

/// A summary of the effects of a stretch of regex.
///
/// This includes a bounding box of all squares visited by any path the regex
/// matches, and the set of deltas that could result from following any such
/// path.
#[derive(Debug)]
struct Summary {
    bounds: DeltaBounds,
    deltas: DeltaSet,
}

impl Summary {
    fn initial() -> Summary {
        let mut deltas = DeltaSet::new();
        deltas.insert((0, 0));
        Summary {
            bounds: (0..1, 0..1),
            deltas,
        }
    }

    fn from_run(run: &Run) -> Summary {
        let (delta, bounds) = run_delta_bounds(run);
        let mut deltas = DeltaSet::new();
        deltas.insert(delta);
        Summary { bounds, deltas }
    }

    fn concat(&self, right: &Summary) -> Summary {
        let summary = Summary {
            bounds: self.deltas.iter().fold(self.bounds.clone(), |acc, &delta| {
                union_bounds(&acc, &shift_bounds(&right.bounds, delta))
            }),
            deltas: DeltaSet::from_iter(
                cartesian_product(&self.deltas, &right.deltas).map(|(a, b)| concat_deltas(a, b)),
            ),
        };
        //eprintln!("  Concat of {:?}", self);
        //eprintln!("  with      {:?}", right);
        //eprintln!("  is        {:?}", summary);
        summary
    }

    fn union(mut self, right: &Summary) -> Summary {
        self.bounds = union_bounds(&self.bounds, &right.bounds);
        self.deltas.extend(&right.deltas);
        self
    }
}

impl Dir {
    fn reverse(self) -> Dir {
        match self {
            Dir::North => Dir::South,
            Dir::South => Dir::North,
            Dir::East => Dir::West,
            Dir::West => Dir::East,
        }
    }
}

impl From<Dir> for Delta {
    fn from(dir: Dir) -> Delta {
        match dir {
            Dir::North => (-1, 0),
            Dir::South => (1, 0),
            Dir::East => (0, 1),
            Dir::West => (0, -1),
        }
    }
}

impl TryFrom<char> for Dir {
    type Error = Error;
    fn try_from(ch: char) -> Result<Dir, Error> {
        match ch {
            'N' => Ok(Dir::North),
            'S' => Ok(Dir::South),
            'E' => Ok(Dir::East),
            'W' => Ok(Dir::West),
            ch => Err(format_err!("Bad direction letter: {:?}", ch)),
        }
    }
}

impl DirSet {
    fn new() -> DirSet {
        DirSet(0)
    }

    fn is_empty(&self) -> bool {
        self.0 == 0
    }

    fn insert(&mut self, dir: Dir) {
        self.0 |= DirSet::bit_from_dir(dir);
    }

    fn has(&self, dir: Dir) -> bool {
        self.0 & DirSet::bit_from_dir(dir) != 0
    }

    fn bit_from_dir(dir: Dir) -> u8 {
        match dir {
            Dir::North => 0b0001,
            Dir::South => 0b0010,
            Dir::East => 0b0100,
            Dir::West => 0b1000,
        }
    }

    fn dir_from_bit(bit: u8) -> Dir {
        match bit {
            0b0001 => Dir::North,
            0b0010 => Dir::South,
            0b0100 => Dir::East,
            0b1000 => Dir::West,
            _ => panic!("DirSet::dir_from_bit: bad direction bit"),
        }
    }
}

impl Iterator for DirSet {
    type Item = Delta;
    fn next(&mut self) -> Option<Delta> {
        if self.0 == 0 {
            return None;
        }
        let bottom = self.0 & !(self.0 - 1);
        self.0 &= !bottom;
        Some(DirSet::dir_from_bit(bottom).into())
    }
}

fn apply_delta(a: Point, b: Delta) -> Point {
    ((a.0 as isize + b.0) as usize, (a.1 as isize + b.1) as usize)
}

fn concat_deltas(a: &Delta, b: &Delta) -> Delta {
    (a.0 + b.0, a.1 + b.1)
}

fn union_bounds(a: &DeltaBounds, b: &DeltaBounds) -> DeltaBounds {
    (union_ranges(&a.0, &b.0), union_ranges(&a.1, &b.1))
}

fn shift_bounds(bounds: &DeltaBounds, delta: Delta) -> DeltaBounds {
    let mut bounds = bounds.clone();

    bounds.0.start += delta.0;
    bounds.0.end += delta.0;

    bounds.1.start += delta.1;
    bounds.1.end += delta.1;

    bounds
}

fn run_delta_bounds(run: &Run) -> (Delta, DeltaBounds) {
    let mut d = (0, 0);
    let mut bounds = (0..1, 0..1);
    for &dir in run {
        d = concat_deltas(&d, &dir.into());
        bounds = union_bounds(&bounds, &(d.0..d.0 + 1, d.1..d.1 + 1));
    }
    //eprintln!("bounds for {:?} = {:?}", run, bounds);
    (d, bounds)
}

impl Concat {
    #[allow(dead_code)]
    fn num_matches(&self) -> usize {
        self.head.iter().map(|(_, alt)| alt.num_matches()).product()
    }

    fn summary(&self) -> Summary {
        let mut summary = Summary::initial();
        for (run, alt) in &self.head {
            summary = summary.concat(&Summary::from_run(run));
            summary = summary.concat(&alt.summary());
        }
        summary = summary.concat(&Summary::from_run(&self.tail));
        //eprintln!("Concat::summary({:?}: {:?}", self, summary);
        summary
    }

    fn trace(&self, map: &mut Map, locs: &PointSet) -> PointSet {
        let locs = self.head.iter().fold(locs.clone(), |acc, (run, alt)| {
            let locs = map.trace_run_from_set(run, &acc);
            let locs = alt.trace(map, locs);
            locs
        });
        map.trace_run_from_set(&self.tail, &locs)
    }
}

impl Alt {
    #[allow(dead_code)]
    fn num_matches(&self) -> usize {
        self.alternatives.iter().map(Concat::num_matches).sum()
    }

    fn summary(&self) -> Summary {
        let summary = self
            .alternatives
            .iter()
            .fold(Summary::initial(), |acc, concat| {
                acc.union(&concat.summary())
            });
        //eprintln!("   Alt::summary({:?}): {:?}", self, summary);
        summary
    }

    fn trace(&self, map: &mut Map, starts: PointSet) -> PointSet {
        let mut ends = PointSet::new();
        for concat in &self.alternatives {
            ends.extend(concat.trace(map, &starts));
        }
        ends
    }
}

impl FromStr for Concat {
    type Err = Error;
    fn from_str(s: &str) -> Result<Concat, Error> {
        fn parse_concat<'a>(cursor: &mut Cursor<'a>) -> Result<Concat, Error> {
            let mut head = Vec::new();
            let mut tail = Vec::new();
            loop {
                match cursor.peek() {
                    Some(ch) if "NSEW".contains(ch) => {
                        cursor.next();
                        tail.push(ch.try_into()?);
                    }
                    Some('(') => {
                        head.push((tail, parse_alt(cursor)?));
                        tail = Vec::new();
                    }
                    _ => break,
                }
            }
            Ok(Concat { head, tail })
        }

        fn parse_alt<'a>(cursor: &mut Cursor<'a>) -> Result<Alt, Error> {
            assert_eq!(cursor.next(), Some('('));

            let mut alternatives = Vec::new();
            loop {
                alternatives.push(parse_concat(cursor)?);
                match cursor.peek() {
                    Some('|') => cursor.next(),
                    Some(')') => break,
                    other => {
                        return Err(format_err!(
                            "regexp alternative contains unexpected: {:?}",
                            other
                        ));
                    }
                };
            }

            assert_eq!(cursor.next(), Some(')'));
            Ok(Alt { alternatives })
        }

        let mut cursor = Cursor::new(s);

        if cursor.next() != Some('^') {
            return Err(format_err!("regexp missing start anchor"));
        }

        let concat = parse_concat(&mut cursor)?;

        if cursor.next() != Some('$') {
            return Err(format_err!("regexp missing end anchor"));
        }

        if cursor.next() != None {
            return Err(format_err!("regexp has more characters after end anchor"));
        }

        Ok(concat)
    }
}

impl fmt::Display for Map {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        fn vertical_doors(
            dir: Dir,
            row: ArrayView1<DirSet>,
            f: &mut fmt::Formatter,
        ) -> Result<(), fmt::Error> {
            for set in row {
                if set.has(dir) {
                    f.write_str("+  ")
                } else {
                    f.write_str("+--")
                }?;
            }
            f.write_str("+\n")
        }

        vertical_doors(Dir::North, self.0.row(0), f)?;
        for row in self.0.genrows() {
            if row[0].has(Dir::West) {
                f.write_str(" ")
            } else {
                f.write_str("|")
            }?;
            for set in row {
                if set.has(Dir::East) {
                    f.write_str("   ")
                } else {
                    f.write_str("  |")
                }?;
            }
            f.write_str("\n")?;
            vertical_doors(Dir::South, row, f)?;
        }
        Ok(())
    }
}

impl Map {
    /// Return the map represented by `regex`, together with the point within
    /// the map at which the regex starts.
    fn from_regex(regex: &Concat) -> (Map, Point) {
        let summary = regex.summary();
        let bounds = &summary.bounds;
        let dim = (
            (bounds.0.end - bounds.0.start) as usize,
            (bounds.1.end - bounds.1.start) as usize,
        );
        assert!(bounds.0.start <= 0 && bounds.1.start <= 0);
        let start = (-bounds.0.start as usize, -bounds.1.start as usize);

        let mut map = Map(Array2::<DirSet>::from_elem(dim, DirSet::new()));

        let mut locs = PointSet::new();
        locs.insert(start);
        regex.trace(&mut map, &locs);
        (map, start)
    }

    fn trace_run_from_set(&mut self, run: &Run, locs: &PointSet) -> PointSet {
        PointSet::from_iter(locs.iter().map(|&loc| self.trace_run(run, loc)))
    }

    fn trace_run(&mut self, run: &Run, mut loc: Point) -> Point {
        for &dir in run {
            // Are we installing a new door?
            let new_door = !self.0[loc].has(dir);
            self.0[loc].insert(dir);
            loc = apply_delta(loc, dir.into());
            // If we were installing a new door to a cell that was previously
            // visited, then the maze contains cycles.
            if new_door && !self.0[loc].is_empty() {
                println!("map is cyclic: {:?}", loc);
            }
            self.0[loc].insert(dir.reverse());
        }
        loc
    }

    fn longest_path_from(&self, start: Point) -> usize {
        // The breadth_first iterator produces all edges in the graph, but we
        // want only the first edge to each node.
        let mut seen = PointSet::new();
        breadth_first(start, |&f| self.0[f].map(move |d| apply_delta(f, d.into())))
            .filter(|(_from, to, _len)| seen.insert(*to))
            .last()
            .unwrap()
            .2
    }
}

fn main() {
    fn summarize(input: &str) -> (Map, Point) {
        let r = Concat::from_str(input).expect("bad summarize input");
        println!("regex: {:.80}", input);
        println!("summary: {:?}", r.summary());

        let (map, start) = Map::from_regex(&r);
        print!("map:\n{}", map);
        println!(
            "longest path from {:?} is {} steps",
            start,
            map.longest_path_from(start)
        );
        println!();

        (map, start)
    }

    summarize("^ENWWW(NEEE|SSE(EE|N))$");
    summarize("^(NEWS|WNSE|)$");
    summarize("^ENNWSWW(NEWS|)SSSEEN(WNSE|)EE(SWEN|)NNN$");
    let (map, start) = summarize(INPUT);

    let mut seen = PointSet::new();
    let at_least_1k = breadth_first(start, |&f| map.0[f].map(move |d| apply_delta(f, d.into())))
        .filter(|(_from, to, _len)| seen.insert(*to)) // take only the first edge to each node
        .filter(|(_from, _to, len)| *len >= 1000)
        .count();
    println!(
        "{} rooms can only be reached by passing through at least 1000 doors.",
        at_least_1k
    );
}

extern crate advent_of_code_2018 as aoc;
extern crate ndarray;

use aoc::{cartesian_product, parse_map};
use ndarray::{Array2, Axis};

#[allow(dead_code)]
static TEST_INPUT: &str = include_str!("day-18.test");
#[allow(dead_code)]
static INPUT: &str = include_str!("day-18.input");

type Point = (usize, usize);

fn neighborhood(p: Point, bounds: Point) -> impl Iterator<Item = Point> + Clone {
    cartesian_product(-1..=1, -1..=1).filter_map(move |(dx, dy)| {
        if dx == 0 && dy == 0 {
            return None;
        }
        let x = p.0 as isize + dx;
        let y = p.1 as isize + dy;
        if x < 0 || y < 0 {
            return None;
        }
        let x = x as usize;
        let y = y as usize;

        if x >= bounds.0 || y >= bounds.1 {
            return None;
        }

        Some((x, y))
    })
}

#[derive(Debug, Default)]
struct Census {
    open: usize,
    wooded: usize,
    lumberyard: usize,
}

impl Census {
    fn count(&mut self, ch: char) {
        match ch {
            '.' => self.open += 1,
            '|' => self.wooded += 1,
            '#' => self.lumberyard += 1,
            _ => panic!("unexpected character {:?} in map", ch),
        }
    }
}

fn neighborhood_census(map: &Array2<char>, p: Point) -> Census {
    let mut c = Census::default();
    for pt in neighborhood(p, map.dim()) {
        c.count(map[pt]);
    }
    c
}

fn map_census(map: &Array2<char>) -> Census {
    let mut c = Census::default();
    for row in 0..map.len_of(Axis(0)) {
        for col in 0..map.len_of(Axis(1)) {
            c.count(map[[row, col]]);
        }
    }
    c
}

fn step(prev: &Array2<char>, next: &mut Array2<char>) {
    assert_eq!(prev.dim(), next.dim());

    for row in 0..prev.len_of(Axis(0)) {
        for col in 0..prev.len_of(Axis(1)) {
            let p = (row, col);
            next[p] = match (prev[p], neighborhood_census(prev, p)) {
                ('.', Census { wooded, .. }) if wooded >= 3 => '|',
                ('|', Census { lumberyard, .. }) if lumberyard >= 3 => '#',
                (
                    '#',
                    Census {
                        lumberyard, wooded, ..
                    },
                ) if lumberyard == 0 || wooded == 0 => '.',
                _ => prev[p],
            };
        }
    }
}

fn step_in_place(now: &mut Array2<char>, other: &mut Array2<char>) {
    step(now, other);
    std::mem::swap(now, other);
}

fn main() {
    let mut map = parse_map(INPUT.lines(), '.');
    let mut temp = map.clone();

    let mut slow_map = map.clone();
    let mut slow_map_temp = map.clone();

    let mut slow_gen = 0;
    loop {
        step_in_place(&mut map, &mut temp);
        step_in_place(&mut map, &mut temp);

        step_in_place(&mut slow_map, &mut slow_map_temp);

        slow_gen += 1;

        if map == slow_map {
            break;
        }
    }

    println!("All right! Generation {} is part of a cycle!", slow_gen);

    let mut period = 0;
    loop {
        step_in_place(&mut map, &mut temp);
        period += 1;

        if map == slow_map {
            break;
        }
    }

    println!("Period of repetition is {}", period);

    let additional = 1_000_000_000 - slow_gen;
    let just_as_good_additional = additional % period;

    for _ in 0..just_as_good_additional {
        step_in_place(&mut slow_map, &mut slow_map_temp);
    }

    let census = map_census(&slow_map);
    println!("Final census: {:?}", census);
    println!("value: {}", census.wooded * census.lumberyard);
}

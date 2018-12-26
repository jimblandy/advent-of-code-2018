#[macro_use]
extern crate ndarray;
extern crate advent_of_code_2018 as root;

use ndarray::{Array2, Axis};
use root::{cartesian_product, edge_indexes2, IteratorExt};
use std::str::FromStr;

#[allow(dead_code)]
static TEST_INPUT: &'static str = include_str!("day-06.test");
#[allow(dead_code)]
static INPUT: &'static str = include_str!("day-06.input");

const NEW: usize = std::usize::MAX;
const TIE: usize = std::usize::MAX - 1;

fn abs_difference(a: usize, b: usize) -> usize {
    if a > b {
        a - b
    } else {
        b - a
    }
}

fn manhattan(a: &(usize, usize), b: &(usize, usize)) -> usize {
    abs_difference(a.0, b.0) + abs_difference(a.1, b.1)
}

#[allow(dead_code)]
fn print_map(map: &Array2<usize>) {
    for r in 0..map.len_of(Axis(0)) {
        for c in 0..map.len_of(Axis(1)) {
            print!("{}", {
                let point = map[[r, c]];
                if point == NEW {
                    '*'
                } else if point == TIE {
                    '.'
                } else {
                    (b'a' + (point as u8) % 26) as char
                }
            });
        }
        println!();
    }
}

fn main() {
    let points: Vec<_> = INPUT
        .lines()
        .map(|line| {
            let coords: Vec<_> = line
                .split(',')
                .map(str::trim)
                .map(|c| usize::from_str(c).unwrap())
                .collect();
            assert_eq!(coords.len(), 2);
            (coords[1], coords[0])
        })
        .collect();

    let height = points.iter().map(|(r, _c)| *r).max().unwrap() + 1;
    let width = points.iter().map(|(_r, c)| *c).max().unwrap() + 1;

    println!("(rows, cols) = {:?}", (height, width));

    let map = Array2::from_shape_fn((height, width), |m| {
        points
            .iter()
            .enumerate()
            .unique_min_by_key(|(_i, p)| manhattan(&m, p))
            .map(|(i, _p)| i)
            .unwrap_or(TIE)
    });

    //print_map(&map);

    let mut infinite = vec![false; points.len()];
    for e in edge_indexes2(&map) {
        if map[e] != TIE {
            infinite[map[e]] = true;
        }
    }
    println!("{:?}", infinite);

    let mut areas = vec![0; points.len()];
    azip!(owner (&map) in {
        if owner != TIE && !infinite[owner] {
            areas[owner] += 1;
        }
    });
    println!("{:?}", areas);

    println!(
        "largest area closest: {:?}",
        areas
            .iter()
            .enumerate()
            .unique_max_by_key(|(_owner, &area)| area)
    );

    let close_area = cartesian_product(0..height, 0..width)
        .filter(|m| points.iter().map(|p| manhattan(m, p)).sum::<usize>() < 10000)
        .count();
    println!(
        "Number of grid points with a summed distance < 10000: {}",
        close_area
    );
}

extern crate advent_of_code_2018 as aoc;

use aoc::Manhattan;
use std::collections::HashSet;
use std::mem::replace;

type Point = (i32, i32, i32, i32);

fn count_constellations(points: &[Point]) {
    let mut parent: Vec<usize> = (0..points.len()).collect();

    for new in 0..points.len() {
        for friend in 0..new {
            if points[new].manhattan(points[friend]) > 3 {
                continue;
            }

            let mut end = friend;
            while parent[end] != end {
                end = parent[end];
            }
            parent[end] = new;
        }
    }

    let mut constellations = HashSet::new();
    for point in 0..points.len() {
        let mut end = point;
        while parent[end] != end {
            end = parent[end];
        }

        let mut retrace = point;
        while parent[retrace] != retrace {
            retrace = replace(&mut parent[retrace], end);
        }
        assert_eq!(retrace, end);

        constellations.insert(end);
    }

    println!("{} distinct constellations.", constellations.len());
    for constellation in &constellations {
        print!("Constellation {}:", constellation);
        for (i, parent) in parent.iter().enumerate() {
            if parent == constellation {
                print!(" {}", i);
            }
        }
        println!();
    }
    println!();
}

fn main() {
    count_constellations(&[
        (0, 0, 0, 0),
        (3, 0, 0, 0),
        (0, 3, 0, 0),
        (0, 0, 3, 0),
        (0, 0, 0, 3),
        (0, 0, 0, 6),
        (9, 0, 0, 0),
        (12, 0, 0, 0),
    ]);

    count_constellations(&[
        (-1, 2, 2, 0),
        (0, 0, 2, -2),
        (0, 0, 0, -2),
        (-1, 2, 0, 0),
        (-2, -2, -2, 2),
        (3, 0, 2, -1),
        (-1, 3, 2, 2),
        (-1, 0, -1, 0),
        (0, 2, 1, -2),
        (3, 0, 0, 0),
    ]);

    count_constellations(&[
        (1, -1, 0, 1),
        (2, 0, -1, 0),
        (3, 2, -1, 0),
        (0, 0, 3, 1),
        (0, 0, -1, -1),
        (2, 3, -2, 0),
        (-2, 2, 0, 0),
        (2, -2, 0, -1),
        (1, -1, 0, -1),
        (3, 2, 0, 2),
    ]);

    count_constellations(&[
        (1, -1, -1, -2),
        (-2, -2, 0, 1),
        (0, 2, 1, 3),
        (-2, 3, -2, 1),
        (0, 2, 3, -2),
        (-1, -1, 1, -2),
        (0, -2, -1, 0),
        (-2, 2, 3, -1),
        (1, 2, 2, 0),
        (-1, -2, 0, -2),
    ]);

    count_constellations(&include!("day-25.input"));
}

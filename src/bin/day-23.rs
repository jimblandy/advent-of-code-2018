extern crate advent_of_code_2018 as aoc;

use aoc::Manhattan;

type Point = (isize, isize, isize);

#[derive(Debug)]
struct Nanobot {
    pos: Point,
    radius: isize,
}

static INPUT: &[Nanobot] = &include!("day-23.input");

fn main() {
    let strongest = INPUT.iter().max_by_key(|n| n.radius).unwrap();

    println!("Strongest nanobot: {:?}", strongest);

    println!(
        "Number of nanobots in range: {}",
        INPUT
            .iter()
            .filter(|nanobot| strongest.pos.manhattan(nanobot.pos) <= strongest.radius)
            .count()
    );
}

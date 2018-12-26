#[allow(dead_code)]
static TEST_INPUT: &'static str = include_str!("day-10.test");
#[allow(dead_code)]
static INPUT: &'static str = include_str!("day-10.input");

extern crate advent_of_code_2018;

use advent_of_code_2018::splits;
use std::ops::Range;
use std::str::FromStr;

#[derive(Clone, Debug)]
struct Light {
    position: (i32, i32),
    velocity: (i32, i32),
}

fn step(lights: &mut [Light], scale: i32) {
    for light in lights {
        light.position.0 += scale * light.velocity.0;
        light.position.1 += scale * light.velocity.1;
    }
}

fn bounds(lights: &[Light]) -> (Range<i32>, Range<i32>) {
    (
        Range {
            start: lights.iter().map(|l| l.position.0).min().unwrap(),
            end: lights.iter().map(|l| l.position.0).max().unwrap() + 1,
        },
        Range {
            start: lights.iter().map(|l| l.position.1).min().unwrap(),
            end: lights.iter().map(|l| l.position.1).max().unwrap() + 1,
        },
    )
}

fn plot(lights: &[Light]) {
    let b = bounds(lights);

    let mut positions: Vec<(i32, i32)> = lights.iter().map(|l| l.position).collect();
    positions.sort_by_key(|p| (p.1, p.0));
    let mut next = 0;

    for row in b.1 {
        for col in b.0.clone() {
            if next < positions.len() && positions[next] == (col, row) {
                print!("#");
                while next < positions.len() && positions[next] == (col, row) {
                    next += 1;
                }
            } else {
                print!(".");
            }
        }
        println!();
    }
    println!();
}

fn area(lights: &[Light]) -> usize {
    let (h, v) = bounds(lights);
    (h.end - h.start) as usize * (v.end - v.start) as usize
}

fn main() {
    let mut lights: Vec<Light> = INPUT
        .lines()
        .map(|line| {
            let fields = splits(&line, "position=<_,_> velocity=<_,_>", i32::from_str)
                .expect("failed to parse fields");
            Light {
                position: (fields[0], fields[1]),
                velocity: (fields[2], fields[3]),
            }
        })
        .collect();

    let mut a = area(&lights);
    for secs in 0.. {
        step(&mut lights, 1);
        let new_area = area(&lights);
        if new_area > a {
            step(&mut lights, -1);
            plot(&lights);
            println!("elapsed time {} secs", secs);
            break;
        }
        a = new_area;
    }
}

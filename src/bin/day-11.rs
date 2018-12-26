extern crate advent_of_code_2018;
extern crate ndarray;

use advent_of_code_2018::cartesian_product;
use ndarray::Array2;
use std::cmp::min;

fn add2<T>(a: (T, T), b: (T, T)) -> (T, T)
where
    T: std::ops::Add<T, Output = T>,
{
    (a.0 + b.0, a.1 + b.1)
}

fn power((x, y): (usize, usize), serial: usize) -> i32 {
    let rack_id = x + 10;
    let power = rack_id * y;
    let power2 = power + serial;
    let power3 = power2 * rack_id;
    /*
        power3 = (((x + 10) * y) + serial) * (x + 10);
        power3 = (x*y + 10*y + serial) * (x + 10);
        power3 = (x*x*y + 10*x*y + serial*x + 10*x*y + 100*y + 10*serial);
        power3 = (x*x*y + 20*x*y + serial*x + 100*y + 10*serial);

        power3 = y*(x*x + 20*x + 100) + serial*x + 10*serial;

        inc(y) = (x*x + 20*x + 100);
    */
    (power3 as i32 % 1000 / 100) - 5
}

fn power_3x3(ul: (usize, usize), serial: usize) -> i32 {
    cartesian_product(0..3, 0..3)
        .map(move |d| power(add2(ul, d), serial))
        .sum()
}

#[test]
fn test_power() {
    assert_eq!(power((3, 5), 8), 4);
    assert_eq!(power((122, 79), 57), -5);
    assert_eq!(power((217, 196), 39), 0);
    assert_eq!(power((101, 153), 71), 4);

    assert_eq!(power_3x3((33, 45), 18), 29);
}

fn main() {
    println!(
        "{:?}",
        cartesian_product(1..=298, 1..=298).max_by_key(|&pt| power_3x3(pt, 18))
    );
    println!(
        "{:?}",
        cartesian_product(1..=298, 1..=298).max_by_key(|&pt| power_3x3(pt, 42))
    );
    println!(
        "{:?}",
        cartesian_product(1..=298, 1..=298).max_by_key(|&pt| power_3x3(pt, 7689))
    );

    let serial = 7689;

    // Northwest sums: the element at (x,y) holds the sum of all powers in the
    // rectangle with corners at (1,1) and (x,y) - that is, all the powers to
    // the northwest of that cell. Note, the problem uses 1-based indexing, but
    // ndarray does not, so we add a top row / left column of zeros.
    let mut nw_sums = Array2::<i32>::zeros((301, 301));
    for r in 1..=300 {
        let mut row_sum = 0;
        for c in 1..=300 {
            row_sum += power((c, r), serial);
            nw_sums[(r, c)] = row_sum + nw_sums[(r - 1, c)];
        }
    }

    let mut best = (0, (0, 0, 0));
    // (r, c) is the lower-right corner of the square.
    for r in 1..=300 {
        for c in 1..=300 {
            for size in 0..min(r, c) {
                let square_power =
                    nw_sums[(r, c)] - nw_sums[(r - size, c)] - nw_sums[(r, c - size)]
                        + nw_sums[(r - size, c - size)];
                if square_power > best.0 {
                    best = (square_power, (c - size + 1, r - size + 1, size));
                }
            }
        }
    }

    println!("most powerful square: {:?}, power {}", best.1, best.0);
}

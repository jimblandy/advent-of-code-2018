#![feature(euclidean_division)]
#![allow(dead_code)]

extern crate advent_of_code_2018;

use advent_of_code_2018::ring::Ring;

/// Play an `p`-player, `n`-marble game (not counting the 'zero' marble), and
/// return a vector of the scores of each player.
fn play(p: usize, n: usize) -> Vec<usize> {
    let mut scores = vec![0; p];
    let mut circle = Ring::new(0);
    for i in 1..=n {
        let player = (i + p - 1) % p;
        if i % 23 == 0 {
            scores[player] += i;
            circle.rotate_backward(7);
            let (front, remainder) = circle.pop_front();
            scores[player] += front;
            circle = remainder.unwrap(); // circle should never become empty
        } else {
            circle.rotate_forward(2);
            circle.insert_at_front(i);
        }
        if i % 1000 == 0 {
            eprint!(".");
        }
    }
    eprintln!();
    println!(
        "{} players; last marble is worth {} points; high score is {}",
        p,
        n,
        scores.iter().max().unwrap()
    );
    scores
}

fn main() {
    println!("{:?}", play(9, 25));
    play(10, 1618);
    play(13, 7999);
    play(17, 1104);
    play(21, 6111);
    play(30, 5807);
    play(464, 71730);
    play(464, 71730 * 100);
}

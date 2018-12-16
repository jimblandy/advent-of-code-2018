#![feature(euclidean_division)]
#![allow(dead_code)]

#[derive(Debug)]
struct Circle {
    marbles: Vec<usize>,
    current: usize,
    next: usize,
}

impl Circle {
    fn new() -> Circle {
        Circle {
            marbles: vec![0],
            current: 0,
            next: 1,
        }
    }

    /// Take the next turn in the game. Return the number of points earned.
    fn next(&mut self) -> usize {
        let mut score;
        if self.next % 23 == 0 {
            score = self.next;
            self.current = (self.current as isize - 7).mod_euc(self.marbles.len() as isize) as usize;
            score += self.marbles.remove(self.current);
        } else {
            let insert_after = (self.current + 1) % self.marbles.len();
            self.marbles.insert(insert_after + 1, self.next);
            self.current = insert_after + 1;
            score = 0;
        }
        self.next += 1;
        score
    }
}

fn play(players: usize, marbles: usize) -> Vec<usize> {
    let mut scores = vec![0; players];
    let mut circle = Circle::new();

    for marble in 0..marbles {
        let player = marble % players;
        scores[player] += circle.next();
        if marble % 10_000 == 0 {
            eprint!(".");
        }
    }
    eprintln!();

    println!("{} players; last marble {}; scores:", players, marbles);
    println!("high score: {}", scores.iter().max().unwrap());
    println!("{:?}", scores);
    println!();
    scores
}

fn main() {
    play(10, 1618);
    play(13, 7999);
    play(17, 1104);
    play(21, 6111);
    play(30, 5807);
    play(464, 71730);
}

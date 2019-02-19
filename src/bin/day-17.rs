#![allow(dead_code)]
#![feature(range_contains)]

extern crate advent_of_code_2018 as aoc;
#[macro_use]
extern crate failure;
extern crate itertools;
extern crate ndarray;

use aoc::union_ranges;
use failure::Error;
use itertools::Itertools;
use ndarray::{Array2, Axis};
use std::io::{BufWriter, Write};
use std::ops::Range;
use std::str::FromStr;

#[derive(Clone, Debug, Eq, PartialEq)]
struct Vein {
    x: Range<usize>,
    y: Range<usize>,
}

type Point = (usize, usize);

/// A map. Contents are:
/// - `' '`: empty space
/// - `'#'`: clay
/// - `'-'`: water that has no outlet. If more water falls on this, it piles up.
/// - `'~'`: water that can flow. If more water falls on this, it does not pile up.
type Map = Array2<char>;

fn has_outlet(map: &Map, p: Point) -> bool {
    p.0 >= map.len_of(Axis(0)) || map[p] == '~'
}

fn is_water(map: &Map, p: Point) -> bool {
    map[p] == '-' || map[p] == '~'
}

#[allow(dead_code)]
static TEST_INPUT: &str = include_str!("day-17.test");
#[allow(dead_code)]
static INPUT: &str = include_str!("day-17.input");

fn display(map: &Array2<char>, bounds: &Vein) -> Result<(), Error> {
    let stdout = std::io::stdout();
    let mut handle = BufWriter::new(stdout.lock());
    for r in bounds.y.clone() {
        for c in bounds.x.clone() {
            write!(handle, "{}", map[[r, c]])?;
        }
        writeln!(handle)?;
    }
    Ok(())
}

impl FromStr for Vein {
    type Err = Error;
    fn from_str(s: &str) -> Result<Vein, Error> {
        let comma = s
            .find(',')
            .ok_or_else(|| format_err!("missing comma in vein: {:?}", s))?;
        let eq1 = s[..comma].trim();
        let eq2 = s[comma + 1..].trim();
        if eq1.len() < 2 || &eq1[1..2] != "=" || eq2.len() < 2 || &eq2[1..2] != "=" {
            return Err(format_err!("missing '=' in vein: {:?}", s));
        }
        let (xeq, yeq) = match (&eq1[..1], &eq2[..1]) {
            ("x", "y") => (&eq1[2..], &eq2[2..]),
            ("y", "x") => (&eq2[2..], &eq1[2..]),
            _ => return Err(format_err!("odd axis labels in vein: {:?}", s)),
        };

        fn parse_range(s: &str) -> Result<Range<usize>, Error> {
            match s.find("..") {
                Some(range) => Ok(Range {
                    start: usize::from_str(&s[..range])?,
                    end: usize::from_str(&s[range + 2..])? + 1,
                }),
                None => {
                    let val = usize::from_str(s)?;
                    Ok(Range {
                        start: val,
                        end: val + 1,
                    })
                }
            }
        }

        Ok(Vein {
            x: parse_range(xeq)?,
            y: parse_range(yeq)?,
        })
    }
}

struct VeinIter {
    vein: Vein,
    next: Point,
}

impl Iterator for VeinIter {
    type Item = Point;
    fn next(&mut self) -> Option<Point> {
        if !self.vein.x.contains(&self.next.0) || !self.vein.y.contains(&self.next.1) {
            return None;
        }

        let result = Some(self.next);

        self.next.0 += 1;
        if self.next.0 >= self.vein.x.end {
            self.next = (self.vein.x.start, self.next.1 + 1);
        }

        result
    }
}

impl IntoIterator for &Vein {
    type Item = Point;
    type IntoIter = VeinIter;
    fn into_iter(self) -> VeinIter {
        VeinIter {
            vein: self.clone(),
            next: (self.x.start, self.y.start),
        }
    }
}

#[test]
#[rustfmt::skip]
fn test_vein_fromstr() -> Result<(), Error> {
    assert!(Vein::from_str("").is_err());
    assert!(Vein::from_str(",").is_err());
    assert!(Vein::from_str("1..,2").is_err());

    assert_eq!(Vein::from_str("x=10,y=15")?,
               Vein { x: 10..11, y: 15..16 });
    assert_eq!(Vein::from_str("x=5..8,y=15..18")?,
               Vein { x: 5..9, y: 15..19 });
    assert_eq!(Vein::from_str("y=5,x=15..18")?,
               Vein { x: 15..19, y: 5..6 });

    Ok(())
}

fn above(p: Point) -> Point {
    (p.0 - 1, p.1)
}

fn below(p: Point) -> Point {
    (p.0 + 1, p.1)
}

fn left(p: Point) -> Point {
    (p.0, p.1 - 1)
}

fn right(p: Point) -> Point {
    (p.0, p.1 + 1)
}

/// Water has reached `p` in `map` for the first time. Fill all tiles that flow
/// from `p` can reach.
///
/// Use `bounds` to display the map.
fn vertical(map: &mut Array2<char>, bounds: &Vein, p: Point) {
    let mut f = p;
    assert_eq!(map[f], ' ');
    loop {
        f.0 += 1;

        if f.0 >= map.len_of(Axis(0)) || map[f] != ' ' {
            break;
        }
    }

    if f.0 < map.len_of(Axis(0)) {
        while f.0 > p.0 && !has_outlet(map, f) {
            f.0 -= 1;
            horizontal(map, bounds, f);
        }
    }

    while f.0 > p.0 && has_outlet(map, f) {
        f.0 -= 1;
        map[f] = '~';
    }

    assert_eq!(f, p);
}

/// Water has reached point `p` in `map` for the first time, but there is no
/// outlet below that point. Flow left and right from `p`, filling the area
/// with either blocked or unblocked symbols as appropriate.
fn horizontal(map: &mut Array2<char>, bounds: &Vein, p: Point) {
    // Scan to the left and right for things to fall into, recursing as
    // appropriate. We should never reach the edge, because we added extra
    // columns at the left and right, and we always fall off the bottom.
    assert!(p.0 + 1 < map.len_of(Axis(0)));
    let mut infinite = false;

    let mut l = p;
    while map[l] == ' ' {
        if map[below(l)] == ' ' {
            vertical(map, bounds, below(l));
        }
        if has_outlet(map, below(l)) {
            infinite = true;
            l.1 -= 1;
            break;
        }
        l.1 -= 1;
    }

    let mut r = (p.0, p.1 + 1);
    while map[r] == ' ' {
        if map[below(r)] == ' ' {
            vertical(map, bounds, below(r));
        }
        if has_outlet(map, below(r)) {
            infinite = true;
            r.1 += 1;
            break;
        }
        r.1 += 1;
    }

    for c in (l.1 + 1)..r.1 {
        map[[p.0, c]] = if infinite { '~' } else { '-' };
    }
}

fn main() -> Result<(), Error> {
    let veins = INPUT
        .lines()
        .map(|line| Vein::from_str(line).unwrap_or_else(|e| panic!("error parsing input: {}", e)))
        .collect::<Vec<_>>();
    let mut bounds = veins
        .iter()
        .cloned()
        .fold1(|a, b| Vein {
            x: union_ranges(&a.x, &b.x),
            y: union_ranges(&a.y, &b.y),
        })
        .expect("empty veins list?");

    // Add a column on either side, for flowing down the edge.
    bounds.x.start -= 1;
    bounds.x.end += 1;

    println!("map dimensions: {:?}", bounds);
    let mut map = Array2::<char>::from_elem((bounds.y.end, bounds.x.end), ' ');
    for vein in veins {
        for (x, y) in &vein {
            map[[y, x]] = '#';
        }
    }

    //display(&map, &bounds)?;
    vertical(&mut map, &bounds, (1, 500));
    println!();
    //display(&map, &bounds)?;

    let mut water = 0;
    for r in bounds.y.clone() {
        for c in bounds.x.clone() {
            if is_water(&map, (r, c)) {
                water += 1;
            }
        }
    }

    println!("Total water: {}", water);

    let mut retained = 0;
    for r in bounds.y.clone() {
        for c in bounds.x.clone() {
            if map[[r, c]] == '-' {
                retained += 1;
            }
        }
    }

    println!("Total water: {}", retained);

    Ok(())
}

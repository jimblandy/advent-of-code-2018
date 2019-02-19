#![feature(range_is_empty)]

extern crate ndarray;

use ndarray::{Array2, Axis};
use std::cmp::{max, min, Ordering};
use std::ops::{Add, Range, Sub};

pub mod astar;
pub mod astar_weighted;
pub mod bfs;
pub mod enclose;
pub mod intersection;
pub mod machine;
pub mod ring;
pub mod unfold;

pub trait IteratorExt: Iterator {
    fn unique_min_by_key<B, F>(self, f: F) -> Option<Self::Item>
    where
        B: Ord,
        F: FnMut(&Self::Item) -> B;

    fn unique_max_by_key<B, F>(self, f: F) -> Option<Self::Item>
    where
        B: Ord,
        F: FnMut(&Self::Item) -> B;
}

impl<I: Iterator> IteratorExt for I {
    /// Return the item of `self` for which `f` returns the lowest value, but if
    /// there is a tie, return `None`.
    fn unique_min_by_key<B, F>(mut self, mut f: F) -> Option<Self::Item>
    where
        B: Ord,
        F: FnMut(&Self::Item) -> B,
    {
        let mut best = match self.next() {
            None => return None,
            Some(b) => b,
        };
        let mut tied = false;

        for item in self {
            match f(&item).cmp(&f(&best)) {
                Ordering::Greater => (),
                Ordering::Equal => {
                    tied = true;
                }
                Ordering::Less => {
                    best = item;
                    tied = false;
                }
            }
        }

        if tied {
            None
        } else {
            Some(best)
        }
    }

    /// Return the item of `self` for which `f` returns the greatest value, but
    /// if there is a tie, return `None`.
    fn unique_max_by_key<B, F>(mut self, mut f: F) -> Option<Self::Item>
    where
        B: Ord,
        F: FnMut(&Self::Item) -> B,
    {
        let mut best = match self.next() {
            None => return None,
            Some(b) => b,
        };
        let mut tied = false;

        for item in self {
            match f(&item).cmp(&f(&best)).reverse() {
                Ordering::Greater => (),
                Ordering::Equal => {
                    tied = true;
                }
                Ordering::Less => {
                    best = item;
                    tied = false;
                }
            }
        }

        if tied {
            None
        } else {
            Some(best)
        }
    }
}

pub fn select_iter<T, I, J>(which: bool, i: I, j: J) -> SelectIter<I, J>
where
    I: Iterator<Item = T>,
    J: Iterator<Item = T>,
{
    if which {
        SelectIter::I(i)
    } else {
        SelectIter::J(j)
    }
}

pub enum SelectIter<I, J> {
    I(I),
    J(J),
}

impl<T, I, J> Iterator for SelectIter<I, J>
where
    I: Iterator<Item = T>,
    J: Iterator<Item = T>,
{
    type Item = T;
    fn next(&mut self) -> Option<T> {
        match self {
            SelectIter::I(i) => i.next(),
            SelectIter::J(j) => j.next(),
        }
    }
}

/// Return an iterator over the indexes of the elements at the edges of `array`.
/// The edges appear in the same order they would in a row-major traversal.
pub fn edge_indexes2<E>(array: &Array2<E>) -> impl Iterator<Item = [usize; 2]> {
    use std::iter::once;

    let height = array.len_of(Axis(0));
    let width = array.len_of(Axis(1));
    // top row
    (0..width)
        .map(|c| [0, c])
        // sides
        .chain((1..height - 1).flat_map(move |r| once([r, 0]).chain(once([r, width - 1]))))
        // bottom
        .chain((0..width).map(move |c| [height - 1, c]))
}

pub fn cartesian_product<A, B>(a: A, b: B) -> impl Iterator<Item = (A::Item, B::Item)> + Clone
where
    A: IntoIterator,
    B: IntoIterator,
    A::Item: Clone,
    A::IntoIter: Clone,
    B::IntoIter: Clone,
{
    let a = a.into_iter();
    let b = b.into_iter();
    a.flat_map(move |i| b.clone().map(move |j| (i.clone(), j)))
}

/// Split `input` into fields separated by the non-underscore sections of
/// `seps`, trim any surrounding whitespace, apply `parser` to each, and return
/// the result as a vector of success values from `parser`.
pub fn splits<'a, T, E, P>(mut input: &'a str, seps: &str, mut parser: P) -> Result<Vec<T>, E>
where
    P: FnMut(&str) -> Result<T, E>,
{
    let mut seps = seps.split('_');
    let mut fields = Vec::new();

    let first = seps.next().expect("seps doesn't even have one separator");
    assert!(input.starts_with(first));
    input = &input[first.len()..];

    for sep in seps {
        let end = input.find(sep).expect("splits: separator not found");
        fields.push(parser(input[..end].trim())?);
        input = &input[end + sep.len()..];
    }
    Ok(fields)
}

/// Return an iterator over the first consecutive run of `iter`'s items
/// indicated by `start`.
///
/// Drop the initial run of items from `iter` until `start(item)` returns
/// `Some(extend)`, for some closure `extend`. Then, produce `item` and all
/// further items from `iter` for as long as `extend` returns true. Once
/// `extend` return `false`, iteration is over.
pub fn first_run<'a, I, S, E>(iter: I, start: S) -> FirstRun<I, S, E>
where
    I: Iterator,
    S: 'a + FnMut(&I::Item) -> Option<E>,
    E: 'a + FnMut(&I::Item) -> bool,
{
    FirstRun::NotYet { iter, start }
}

pub enum FirstRun<I, S, E> {
    NotYet { iter: I, start: S },
    Extending { iter: I, extend: E },
    Done,
}

impl<I, S, E> Iterator for FirstRun<I, S, E>
where
    I: Iterator,
    S: FnMut(&I::Item) -> Option<E>,
    E: FnMut(&I::Item) -> bool,
{
    type Item = I::Item;
    fn next(&mut self) -> Option<Self::Item> {
        match std::mem::replace(self, FirstRun::Done) {
            FirstRun::NotYet {
                mut iter,
                mut start,
            } => {
                // Drop items until we get one that `start` likes.
                while let Some(item) = iter.next() {
                    if let Some(extend) = start(&item) {
                        *self = FirstRun::Extending { iter, extend };
                        return Some(item);
                    }
                }
                *self = FirstRun::Done;
                None
            }
            FirstRun::Extending {
                mut iter,
                mut extend,
            } => {
                if let Some(item) = iter.next() {
                    if extend(&item) {
                        *self = FirstRun::Extending { iter, extend };
                        Some(item)
                    } else {
                        *self = FirstRun::Done;
                        None
                    }
                } else {
                    *self = FirstRun::Done;
                    None
                }
            }
            FirstRun::Done => None,
        }
    }
}

#[test]
fn test_first_run() {
    assert_eq!(
        first_run(0..10, |n: &i32| if *n > 3 {
            Some(|m: &i32| *m < 7)
        } else {
            None
        })
        .collect::<Vec<_>>(),
        vec![4, 5, 6]
    );
}

/// Given an iterable producing string slices representing rows of some sort of
/// map, return the height and width of the map, as a pair `(rows, columns)`.
pub fn map_bounds<'a, I>(lines: I) -> (usize, usize)
where
    I: IntoIterator<Item = &'a str>,
{
    lines.into_iter().fold((0, 0), |(rows, columns), line| {
        (rows + 1, max(columns, line.len()))
    })
}

/// Given an iterable producing string slices representing rows of some sort of
/// map, and whose iterator is clonable, return the map as an `Array2<char>`.
/// Use `default` to pad shorter lines out to the full length.
pub fn parse_map<'a, I>(lines: I, default: char) -> Array2<char>
where
    I: IntoIterator<Item = &'a str>,
    I::IntoIter: Clone,
{
    let iter = lines.into_iter();
    let mut map = Array2::from_elem(map_bounds(iter.clone()), default);

    for (row, line) in iter.enumerate() {
        for (col, ch) in line.chars().enumerate() {
            map[[row, col]] = ch;
        }
    }

    map
}

pub fn union_ranges<Idx: Ord + Copy>(a: &Range<Idx>, b: &Range<Idx>) -> Range<Idx> {
    min(a.start, b.start)..max(a.end, b.end)
}

pub fn extend_range<Idx: Ord + Copy>(a: Range<Idx>, b: Idx) -> Range<Idx> {
    min(a.start, b)..max(a.end, b)
}

pub struct Cursor<'a>(std::iter::Peekable<std::str::Chars<'a>>);

impl<'a> Cursor<'a> {
    pub fn new(slice: &str) -> Cursor {
        Cursor(slice.chars().peekable())
    }

    pub fn peek(&mut self) -> Option<char> {
        self.0.peek().cloned()
    }
}

impl<'a> Iterator for Cursor<'a> {
    type Item = char;
    fn next(&mut self) -> Option<char> {
        self.0.next()
    }
}

pub fn manhattan<T>(a: T, b: T) -> T
where
    T: PartialOrd + Add<Output = T> + Sub<Output = T>,
{
    if a >= b {
        a - b
    } else {
        b - a
    }
}

pub trait Manhattan {
    type Output;
    fn manhattan(self, b: Self) -> Self::Output;
}

impl<T> Manhattan for (T, T)
where
    T: PartialOrd + Add<Output = T> + Sub<Output = T>,
{
    type Output = T;
    fn manhattan(self, b: (T, T)) -> T {
        manhattan(self.0, b.0) + manhattan(self.1, b.1)
    }
}

impl<T> Manhattan for (T, T, T)
where
    T: PartialOrd + Add<Output = T> + Sub<Output = T>,
{
    type Output = T;
    fn manhattan(self, b: (T, T, T)) -> T {
        manhattan(self.0, b.0) + manhattan(self.1, b.1) + manhattan(self.2, b.2)
    }
}

impl<T> Manhattan for (T, T, T, T)
where
    T: PartialOrd + Add<Output = T> + Sub<Output = T>,
{
    type Output = T;
    fn manhattan(self, b: Self) -> T {
        manhattan(self.0, b.0)
            + manhattan(self.1, b.1)
            + manhattan(self.2, b.2)
            + manhattan(self.3, b.3)
    }
}

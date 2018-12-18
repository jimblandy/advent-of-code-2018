extern crate ndarray;

use std::cmp::Ordering;
use ndarray::{Array2, Axis};

pub mod ring;

pub trait IteratorExt: Iterator {
    fn unique_min_by_key<B, F>(self, f: F) -> Option<Self::Item>
    where B: Ord,
          F: FnMut(&Self::Item) -> B;

    fn unique_max_by_key<B, F>(self, f: F) -> Option<Self::Item>
    where B: Ord,
          F: FnMut(&Self::Item) -> B;
}

impl<I: Iterator> IteratorExt for I {
    /// Return the item of `self` for which `f` returns the lowest value, but if
    /// there is a tie, return `None`.
    fn unique_min_by_key<B, F>(mut self, mut f: F) -> Option<Self::Item>
    where B: Ord,
          F: FnMut(&Self::Item) -> B
    {
        let mut best = match self.next() {
            None => return None,
            Some(b) => b
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
    where B: Ord,
          F: FnMut(&Self::Item) -> B
    {
        let mut best = match self.next() {
            None => return None,
            Some(b) => b
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

/// Return an iterator over the indexes of the elements at the edges of `array`.
/// The edges appear in the same order they would in a row-major traversal.
pub fn edge_indexes2<E>(array: &Array2<E>) -> impl Iterator<Item=[usize; 2]> {
    use std::iter::once;

    let height = array.len_of(Axis(0));
    let width = array.len_of(Axis(1));
    // top row
    (0 .. width).map(|c| [0, c])
        // sides
        .chain((1 .. height - 1).flat_map(move |r| once([r, 0]).chain(once([r, width - 1]))))
        // bottom
        .chain((0 .. width).map(move |c| [height - 1, c]))
}

pub fn cartesian_product<A, B>(a: A, b: B) -> impl Iterator<Item=(A::Item, B::Item)>
where A: IntoIterator,
      B: IntoIterator,
      A::Item: Clone,
      B::IntoIter: Clone,
{
    let a = a.into_iter();
    let b = b.into_iter();
    a.flat_map(move |i| b.clone().map(move |j| (i.clone(), j)))
}

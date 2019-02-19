#![feature(euclidean_division)]

extern crate advent_of_code_2018 as aoc;

use std::cmp::{max, min};
use std::iter::FromIterator;

type Point = (isize, isize, isize);

#[derive(Debug)]
struct Nanobot {
    pos: Point,
    radius: isize,
}

#[allow(dead_code)]
static TEST_INPUT: &[Nanobot] = &include!("day-23b.test");

#[allow(dead_code)]
static INPUT: &[Nanobot] = &include!("day-23.input");

/// Pairwise product of two `Point`s.
fn mul(a: Point, b: Point) -> Point {
    (a.0 * b.0, a.1 * b.1, a.2 * b.2)
}

/// Dot product of two `Point`s.
fn dot(a: Point, b: Point) -> isize {
    (a.0 * b.0 + a.1 * b.1 + a.2 * b.2)
}

/// An 'orthant' is a `Point` whose components are all either `1` or `-1`. We
/// number the orthants from 0 to 7, where a bit set in the orthant number
/// represents a `-1` component in the corresponding position:
/// - orthant 0 is (1, 1, 1)
/// - orthant 1 is (1, 1, -1)
/// - orthant 2 is (1,-1, 1)
/// - orthant 7 is (-1, -1, -1)
///
/// Each face of an axis-aligned octahedron is a plane whose points `p` satisfy
/// the equation
///
///     dot(orthant(i), p) == b
///
/// where `i` identifies a particular face, and `b` is a function of how far
/// that face is from the origin. The orthant determines the orientation of the
/// face.
fn orthant(orthant: usize) -> Point {
    if orthant >= 8 {
        panic!("Not a 3d orthant number: {}", orthant);
    }

    let flag_to_sign = |bit| 1 - 2 * (orthant & bit != 0) as isize;

    (flag_to_sign(4), flag_to_sign(2), flag_to_sign(1))
}

/// A type representing the inter**section** of any number of axis-aligned
/// **octa**hedra.
///
/// By an 'axis-aligned octahedron' (or 'AAO'), we mean one whose three major
/// axes (from any vertex to its opposite) are parallel to the x, y, and z axes
/// and all of the same length. (The octahedron's faces are thus obviously not
/// orthogonal to any axis.) The set of points within a given Manhattan distance
/// of a given point form an axis-aligned octahedron.
///
/// Each face of an AAO is a plane `±x + ±y + ±z = b`, with each of the eight
/// faces corresponding to one of the eight possible choices for the
/// coefficients (1 or -1) on the coordinates. Each face has its own value for
/// `b`. In Rust, the plane would be the set of `Point`s `p` such that:
///
///     dot(orthant(i), p) == b
///
/// where `i` is a number from `0` to `7` representing a particular choice for
/// the three signs: the `2²`, `2¹`, and `2⁰` bits represent the coefficients
/// for `x`, `y`, and `z`, with a `0` bit representing a coefficient of `1`, and
/// a `1` bit representing a coefficient of `-1`. Thus, orthant number `0`
/// represents coefficients `(1,1,1)`, and orthant `7` is `(-1,-1,-1)`. (An
/// 'orthant' is a generalization of a quadrant.)
///
/// If we take each plane as the boundary of a half-space, the volume of the
/// octahedron is the intersection of the eight halfspaces `±x + ±y + ±z <= b`.
/// (Hence 'b', for 'bound'.) Thus, we can represent any AAO, centered at any
/// point and of any size, as an array of eight values for `b`, where the `i`'th
/// element is the value of `b` for the face whose coefficients are
/// `orthant(i)`.
///
/// We can find the intersection of two AAOs by pairing up the corresponding
/// half-spaces from each and computing the intersection of each pair, simply by
/// taking the lesser of the two values for `b`. Since intersection is
/// associative, the intersection of the eight resulting half-spaces must then
/// be the intersection of the two AAOs. And because this procedure doesn't
/// depend on the input being a regular octahedron, or even an octahedron at
/// all, any pair of spaces represented as arrays of per-orthant `b` values can
/// be intersected in this way. In other words, the set of spaces representable
/// as an array of per-orthant `b` values:
///
/// - includes all axis-aligned octahedra, and
///
/// - is closed under intersection.
///
/// Thus, we call a space represented as an array of per-orthant `b` values an
/// 'octahedron intersection', or an 'OctaSection`.
///
/// Changing a face's value for `b` moves the plane up and down - or
/// equivalently, given the way the faces are oriented, back and forth, or left
/// to right. However, changing `b` cannot affect the face's orientation: that
/// is determined entirely by the coefficients on `x`, `y`, and `z`.
///
/// Since any two planes that differ only in the choice of `b` must be parallel,
/// octasection faces come in parallel pairs: any face's equation can be negated
/// to produce an equation whose signs match one of the other faces - its
/// opposite face, in fact - differing only in `b`. In fact, the face opposite
/// that for orthant number `i` is for orthant `7-i` (or `7 ^ 0b111`,
/// equivalently). Call the space between two parallel faces a `slab`: a plane
/// with depth. The four pairs of parallel faces form four slabs.
///
/// Hence, if `i` is any orthant number, and `-bᵢ > b₇₋ᵢ`, then there is no
/// point `(x,y,z)` that lies within both half-spaces; that slab is empty, and
/// thus the whole octasection is empty. (If `-bᵢ == b₇₋ᵢ`, then the
/// octasection's contents, if any, lie within a single plane.)
///
/// However, even if every slab is non-empty, the octasection may yet represent
/// an empty space. Since the slabs are all mutually non-parallel, any three
/// non-empty slabs have a non-empty intersection. However, that intersection
/// will always be bounded, so the fourth slab may not intersect it.
///
/// The dihedral angle between two adjacent faces of an AAO is roughly 109°,
/// meaning that the planes of any face's three neighbors intersect at a point
/// somewhere beyond the face. Octasections can represent spaces with fewer than
/// eight faces, so the definition of 'adjacent face' is less clear on
/// octasections, but we can say that each face of an octasection is parallel to
/// some face of an AAO. We can use this fact to put an upper bound on values
/// for each `b` that actually contribute to the intersection. Returning to the
/// four-slab construction from above, applying these limits effectively trims
/// each of the first three slabs down to the part that intersects with the
/// fourth slab, minimizing the slabs without changing their intersection. After
/// such a trim, if each slab is still non-empty, then the octasection is
/// non-empty.
///
/// So, since:
///
/// - if for all `i`, `-bᵢ > b₇₋ᵢ`, the intersection is empty; and
///
/// - if for all `i` in a trimmed octasection, if each slab is non-empty, then
///   the octasection is non-empty,
///
/// we have a reliable test for emptyness: trim the octasection, and check that
/// each slab is non-empty.
///
/// To simplify the code, the `OctaSection` type defined here represents *only*
/// trimmed octasections, and every empty `OctaSection` is represented with the
/// `b` values `[-1, 0, 0, 0, 0, 0, 0, 0]`.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
struct OctaSection([isize; 8]);

impl OctaSection {
    fn empty() -> OctaSection {
        OctaSection([-1, 0, 0, 0, 0, 0, 0, 0])
    }

    fn from_bounds(bounds: &[isize; 8]) -> OctaSection {
        let mut trimmed = [0; 8];
        for face in 0..8 {
            // Find the point at which the three faces 'adjacent' to `face`
            // intersect. The three faces adjacent to `face` are conveniently
            // numbered `face ^ 0b001`, `face ^ 0b010`, and `face ^ 0b100`.
            //
            // Note that even lines at 45° angles along integral points may meet
            // at points off the grid:
            //
            // \---+---+---/---+
            // | \ |   | / |   |
            // +---\---/---+---+
            // |   | x |   |   |
            // +---/---\---+---+
            // | / |   | \ |   |
            // /---+---+---\---+
            //
            // So the intersection of the planes may not have integral
            // coordinates. To avoid possibly lossy integer division, we
            // actually pull the division by two out to the very end, and
            // compute the double of each coordinate of the intersection for
            // now.
            let intersection = mul(
                orthant(face),
                (
                    bounds[face ^ 0b010] + bounds[face ^ 0b001],
                    bounds[face ^ 0b100] + bounds[face ^ 0b001],
                    bounds[face ^ 0b100] + bounds[face ^ 0b010],
                ),
            );

            // Now find the `b` value for the plane parallel to `face` that
            // contains `intersection`. Now we can divide, rounding towards +∞,
            // since we're intersecting planes bounded with `<= b`.
            let intersection_bound = (dot(intersection, orthant(face)) + 1).div_euclid(2);

            // Certainly, any points `p` for which
            // `dot(orthant(face), p) > intersection_bound` fall
            // outside the three adjacent faces, and thus are not part of the
            // intersection represented by this OctaSection, so if bounds[face]
            // is greater than that, the excess doesn't contribute to the
            // intersection and can be trimmed.
            trimmed[face] = min(bounds[face], intersection_bound);
        }

        // If this OctaSection is empty, put it in the canonical form.
        for face in 0..4 {
            if -trimmed[face] > trimmed[face ^ 0b111] {
                trimmed = [-1, 0, 0, 0, 0, 0, 0, 0];
                break;
            }
        }

        OctaSection(trimmed)
    }

    /// Return an `OctaSection` representing the axis-aligned octahedron
    /// centered at `center`, and whose vertexes are `radius` away from
    /// `center`.
    fn from_center_radius(center: Point, radius: isize) -> OctaSection {
        if radius < 0 {
            return OctaSection([-1, 0, 0, 0, 0, 0, 0, 0]);
        }

        let mut bounds = [0; 8];

        // Find the `b` value for every face that produces a plane intersecting
        // the center; and then back it away from the center by the given radius.
        // Since we're using `<= b`, increase the enclosed entails increasing `b`.
        for face in 0..8 {
            bounds[face] = dot(orthant(face), center);
            bounds[face] += radius;
        }

        OctaSection(bounds)
    }

    fn is_empty(&self) -> bool {
        self.0 == [-1, 0, 0, 0, 0, 0, 0, 0]
    }

    fn intersection(&self, other: &OctaSection) -> OctaSection {
        let mut intersection = [0; 8];
        for face in 0..8 {
            intersection[face] = min(self.0[face], other.0[face]);
        }
        OctaSection::from_bounds(&intersection)
    }

    fn enclosure(&self, other: &OctaSection) -> OctaSection {
        if self.is_empty() {
            return other.clone();
        }

        if other.is_empty() {
            return self.clone();
        }

        let mut enclosure = [0; 8];
        for face in 0..8 {
            enclosure[face] = max(self.0[face], other.0[face]);
        }
        OctaSection::from_bounds(&enclosure)
    }

    fn shortest_distance_to_origin(&self) -> isize {
        self.0.iter().map(|&b| -min(b, 0)).max().unwrap()
    }
}

#[rustfmt::skip]
#[test]
fn test_octasection() {
    assert_eq!(OctaSection::from_center_radius((0, 0, 0), 1).0, [1, 1, 1, 1, 1, 1, 1, 1]);
    assert_eq!(OctaSection::from_center_radius((10, 0, 0), 1).0, [11, 11, 11, 11, -9, -9, -9, -9]);
    assert_eq!(OctaSection::from_center_radius((0, 10, 0), 1).0, [11, 11, -9, -9, 11, 11, -9, -9]);
    assert_eq!(OctaSection::from_center_radius((0, 0, 10), 1).0, [11, -9, 11, -9, 11, -9, 11, -9]);

    assert_eq!(OctaSection::from_bounds(&[1, 1, 1, 1, 1, 1, 1, 1]).0, [1, 1, 1, 1, 1, 1, 1, 1]);
    assert_eq!(OctaSection::from_bounds(&[10, 1, 1, 1, 1, 1, 1, 1]).0, [3, 1, 1, 1, 1, 1, 1, 1]);
    assert_eq!(OctaSection::from_bounds(&[1, 10, 1, 1, 1, 1, 1, 1]).0, [1, 3, 1, 1, 1, 1, 1, 1]);
    assert_eq!(OctaSection::from_bounds(&[1, 1, 1, 1, 10, 1, 1, 1]).0, [1, 1, 1, 1, 3, 1, 1, 1]);
    assert_eq!(OctaSection::from_bounds(&[1, 1, 1, 1, 1, 1, 1, 10]).0, [1, 1, 1, 1, 1, 1, 1, 3]);
    assert_eq!(OctaSection::from_bounds(&[5, 1, 1, 1, 1, 1, 1, 10]).0, [3, 1, 1, 1, 1, 1, 1, 3]);

    assert_eq!(OctaSection::from_bounds(&[5, 1, 1, 1, 1, 1, 1, -4]).0, [-1, 0, 0, 0, 0, 0, 0, 0]);
    assert_eq!(OctaSection::from_bounds(&[5, 1, 1, 1, 1, 1, 1, -3]).0, [3, 1, 1, -1, 1, -1, -1, -3]);

    assert_eq!(OctaSection::intersection(&OctaSection::from_center_radius((0,0,0), 1),
                                         &OctaSection::from_center_radius((2,0,0), 1)),
               OctaSection::from_center_radius((1,0,0),0));
    assert_eq!(OctaSection::intersection(&OctaSection::from_center_radius((1,2,3), 1),
                                         &OctaSection::from_center_radius((0,0,0), 10)),
               OctaSection::from_center_radius((1,2,3),1));
    assert_eq!(OctaSection::intersection(&OctaSection::from_center_radius((0,0,0), 1),
                                         &OctaSection::from_center_radius((1,0,0), 1)),
               OctaSection::from_bounds(&[1,1,1,1,0,0,0,0]));
    assert_eq!(OctaSection::intersection(&OctaSection::from_center_radius((0,0,0), 1),
                                         &OctaSection::from_center_radius((1,1,0), 1)),
               OctaSection::from_bounds(&[1,1,1,1,1,1,-1,-1]));

    assert_eq!(OctaSection::from_center_radius((0,0,0), 10).shortest_distance_to_origin(), 0);
    assert_eq!(OctaSection::from_center_radius((10,0,0), 5).shortest_distance_to_origin(), 5);
    assert_eq!(OctaSection::from_center_radius((10,1,0), 5).shortest_distance_to_origin(), 6);
    assert_eq!(OctaSection::from_center_radius((10,1,2), 5).shortest_distance_to_origin(), 8);
    assert_eq!(OctaSection::from_center_radius((10,5,0), 5).shortest_distance_to_origin(), 10);
    assert_eq!(OctaSection::from_center_radius((-10,5,0), 5).shortest_distance_to_origin(), 10);
    assert_eq!(OctaSection::from_center_radius((10,-5,0), 5).shortest_distance_to_origin(), 10);
    assert_eq!(OctaSection::from_center_radius((-10,-5,0), 5).shortest_distance_to_origin(), 10);
}

#[allow(unused_variables)]
fn main() {
    let bots = Vec::from_iter(
        INPUT
            .iter()
            .map(|bot| OctaSection::from_center_radius(bot.pos, bot.radius)),
    );

    let enclosure = bots
        .iter()
        .fold(OctaSection::empty(), |e, b| e.enclosure(b));

    let mut index_overlaps: Vec<(usize, usize)> = bots
        .iter()
        .enumerate()
        .map(|(i, bot_i)| {
            let overlaps = bots
                .iter()
                .enumerate()
                .filter(|(j, bot_j)| *j != i && !bot_i.intersection(bot_j).is_empty())
                .count();
            (i, overlaps)
        })
        .collect();
    index_overlaps.sort_by_key(|(i, overlaps)| *overlaps);

    for &(i, overlaps) in &index_overlaps {
        println!("bot {} overlaps {} other bots", i, overlaps);
    }

    const START: usize = 30;
    let intersection = index_overlaps[START..]
        .iter()
        .fold(enclosure.clone(), |int, &(i, _)| int.intersection(&bots[i]));

    if intersection.is_empty() {
        println!("Bots from {} onwards have no intersection.", START);
    } else {
        println!("Bots from {} onwards have a non-empty intersection.", START);
    }

    println!("Intersection: {:?}", intersection);
    println!(
        "Shortest distance from origin to point in intersection: {}",
        intersection.shortest_distance_to_origin()
    );
}

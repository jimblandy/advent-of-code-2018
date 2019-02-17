#![feature(euclidean_division)]

extern crate advent_of_code_2018 as aoc;
#[macro_use]
extern crate lazy_static;

use std::borrow::Cow;
use std::cell::RefCell;
use std::cmp::{max, min};
use std::collections::{HashMap, HashSet};
use std::collections::hash_map::Entry;
use std::hash::Hash;
use std::iter::FromIterator;
use std::rc::Rc;
use std::sync::{atomic, Mutex};

type Point=(isize, isize, isize);

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
    (a.0 * b.0 +
     a.1 * b.1 +
     a.2 * b.2)
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

    let flag_to_sign = |bit| {
        1 - 2 * (orthant & bit != 0) as isize
    };

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
            let intersection = mul(orthant(face),
                                   (bounds[face ^ 0b010] + bounds[face ^ 0b001],
                                    bounds[face ^ 0b100] + bounds[face ^ 0b001],
                                    bounds[face ^ 0b100] + bounds[face ^ 0b010]));

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

    fn outset(&mut self, delta: isize) {
        for elt in &mut self.0 {
            *elt += delta;
        }
    }
}

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
}

static NEXT_ID: atomic::AtomicUsize = atomic::AtomicUsize::new(0);

fn generate_id() -> usize {
    NEXT_ID.fetch_add(1, atomic::Ordering::SeqCst)
}

struct LatticeNode {
    /// The extent of this lattice node, a strict subset of each of its parents'
    /// extents.
    extent: OctaSection,

    /// The number of nanobots whose range is exactly `extent`.
    bots: usize,

    /// All nodes in the lattice whose extents are strict subsets of `extent`,
    /// but not of any other node in this vector.
    ///
    /// It follows that no child contains any other child. If some child1 would
    /// be fully contained within some other child2, then child1 should be a
    /// descendant of child2, not a direct child of this node.
    children: Vec<Rc<RefCell<LatticeNode>>>,

    /// Unique identifier for this node.
    id: usize,
}

impl LatticeNode {
    fn make_enclosing<I: IntoIterator<Item=OctaSection>>(iter: I) -> LatticeNode {
        let enclosure = iter.into_iter().fold(OctaSection::empty(), |e, i| {
            e.enclosure(&i)
        });

        LatticeNode {
            extent: enclosure,
            bots: 0,
            children: vec![],
            id: generate_id(),
        }
    }

    fn from_intersection(extent: OctaSection) -> LatticeNode {
        LatticeNode {
            extent,
            bots: 0,
            children: vec![],
            id: generate_id(),
        }
    }

    fn outset(&mut self, delta: isize) {
        self.extent.outset(delta);
    }

    fn insert(&mut self, mut new: LatticeNode) -> Rc<RefCell<LatticeNode>> {
        // If this node encloses us, this method shouldn't have been called. We
        // checked that it's not equal to us above, so it must be a subset.
        assert!(self.extent != new.extent &&
                self.extent.intersection(&new.extent) == new.extent);

        let mut no_overlap = vec![];
        let mut partial_overlap = vec![];
        let mut contained_by_new = vec![];
        let mut contains_new = None;
        for child in self.children.drain(..) {
            let child_extent = child.borrow().extent.clone();
            let int = child_extent.intersection(&new.extent);

            if int.is_empty() {
                no_overlap.push(child);
            } else if int == new.extent {
                if contains_new.is_none() {
                    contains_new = Some(child);
                } else {
                    partial_overlap.push(child);
                }
            } else if int == child_extent {
                contained_by_new.push(child);
            } else {
                partial_overlap.push(child);
            }
        }

        self.children.extend(no_overlap);
        new.children = contained_by_new;

        if let Some(container) = contains_new {
            let result;
            if container.borrow().extent == new.extent {
                // The new node is exactly equal to container. Merge the two
                // nodes. Intersections between `new` and other children are
                // covered by their existing intersections with `container`.
                container.borrow_mut().bots += new.bots;
                result = container.clone();
            } else {
                // `new` is a strict subset of `container`. Recurse.
                result = container.borrow_mut().insert(new);
            }

            // The container remains this node's child.
            self.children.push(container);

            // We actually don't need to do anything with the `partial_overlap`
            // list. Let `child2` be any element of `partial_overlap`. Since
            // `new ⊆ container`, it follows that `new ∩ child2 ⊆ container`,
            // and hence `new ∩ child2 ⊆ container ∩ child2`. Since `new ∩
            // child2` is non-empty, `container ∩ child2` must be non-empty too.
            // Thus, `container` and `child2` must already have a common
            // descendant representing their intersection. Some direct child of
            // `container` must enclose that intersection, so if we recursively
            // insert `new` into `child`, `new ∩ child2` will end up being a
            // descendant of `child2` as well. (If new == container, then
            // `container ∩ child2` is already a descendant of `child2`.) So we
            // don't need to do any work for `partial_overlap`, beyond making
            // sure they remain our children.
            self.children.extend(partial_overlap);

            return result;
        }

        // Add child nodes for all the partial overlaps. (We don't do this
        // earlier, because we can skip this if `new` is fully contained by some
        // other child.)
        for overlap in &mut partial_overlap {
            let int = overlap.borrow().extent.intersection(&new.extent);
            /*
            int.set_label(format!("({} ∩ {})",
                                  overlap.borrow().extent.get_label(),
                                  new.extent.get_label()));
            */
            let int_node = LatticeNode::from_intersection(int);
            let int_node = overlap.borrow_mut().insert(int_node);
            new.children.push(int_node);
        }

        // Put them back in the list.
        self.children.extend(partial_overlap);

        // The new node is a sibling to the other children.
        let new = Rc::new(RefCell::new(new));
        self.children.push(new.clone());
        return new;
    }

    fn traverse<F>(&self, visitor: &mut F)
        where F: FnMut(&LatticeNode, usize, bool)
    {
        fn visit<F>(node: &LatticeNode, visited: &mut HashSet<usize>, visitor: &mut F, depth: usize)
            where F: FnMut(&LatticeNode, usize, bool)
        {
            let first = visited.insert(node.id);
            visitor(node, depth, first);
            if !first {
                return;
            }

            let depth = depth + 1;
            for child in &node.children {
                visit(&*child.borrow(), visited, visitor, depth);
            }
        }

        let mut visited = HashSet::new();
        visit(self, &mut visited, visitor, 0);
    }
}

trait Labeled: 'static + Clone + Eq + Hash {
    fn table() -> &'static Mutex<HashMap<Self, String>>;

    fn set_label(&self, label: String) {
        let mut lock = Self::table().lock().expect("locking label table for set");
        match lock.entry(self.clone()) {
            Entry::Occupied(mut o) => {
                let combined = format!("{}, {}", o.get(), label);
                o.insert(combined);
            }
            Entry::Vacant(v) => { v.insert(label); }
        }
    }

    fn get_label(&self) -> Cow<'static, str> {
        let lock = Self::table().lock().expect("locking label table for get");
        lock.get(self).map_or(Cow::Borrowed("<unlabeled>"), |l| Cow::Owned(l.to_owned()))
    }
}

lazy_static! {
    static ref OCTASECTION_LABELS: Mutex<HashMap<OctaSection, String>>
        = Mutex::new(HashMap::new());
}

impl Labeled for OctaSection {
    fn table() -> &'static Mutex<HashMap<OctaSection, String>> {
        &OCTASECTION_LABELS
    }
}

fn main() {
    let bots =
        Vec::from_iter(
            INPUT
                .iter()
                .map(|bot| OctaSection::from_center_radius(bot.pos, bot.radius)));

    for (i, bot) in bots.iter().enumerate() {
        bot.set_label(format!("b{}", i));
    }

    for i in 0..min(bots.len(), 8) {
        print!("{}:", i);
        for j in 0..i {
            if !bots[i].intersection(&bots[j]).is_empty() {
                print!(" {}", j);
            }
        }
        println!();
    }

    let mut universe = LatticeNode::make_enclosing(bots.iter().cloned());
    universe.outset(1);
    universe.extent.set_label("universe".to_owned());

    for (i, bot) in bots.into_iter().enumerate() {
        eprintln!("Inserting bot {:?}", i);
        universe.insert(LatticeNode::from_intersection(bot));
    }


        let mut unique = HashSet::new();
        universe.traverse(&mut |node, depth, first| {
            /*
            println!("{:indent$}{}{}: {}", "", node.id,
                     if first { "" } else { "*" },
                     node.extent.get_label(),
                     indent = depth * 4);
            */
            if first {
                assert!(unique.insert(node.extent.clone()))
            }
        });
        println!("{} unique nodes", unique.len());
        println!();

}

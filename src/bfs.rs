use std::collections::{HashSet, VecDeque};
use std::hash::Hash;

/// Iterate over the edges `(from, to, path_length)` of a graph in depth-first
/// order. In the values produced, `from` and `to` are the edge's origin and
/// target, and `path_length` is the total length of the path from the starting
/// node to `to` (that is, including this edge).
///
/// All edges are assumed to have equal cost.
///
/// Because the traversal is breadth-first, edges are produced in order of
/// nondecreasing `path_length`. Eventually, every edge of the graph is
/// produced.
///
/// The graph itself is determined by the `neighbors` function. Given any node,
/// `neighbors` must return an iterator over all its immediate neighbor nodes.
pub fn breadth_first<N, F, I>(start: N, mut neighbors: F) -> BreadthFirst<N, F>
where
    N: Clone + Eq + Hash,
    F: FnMut(&N) -> I,
    I: IntoIterator<Item = N>,
{
    let mut pending = VecDeque::new();
    for neighbor in neighbors(&start) {
        pending.push_back((start.clone(), neighbor, 1));
    }

    let mut visited = HashSet::new();
    visited.insert(start);

    BreadthFirst {
        visited,
        pending,
        neighbors,
    }
}

pub struct BreadthFirst<N, F> {
    visited: HashSet<N>,
    pending: VecDeque<(N, N, usize)>,
    neighbors: F,
}

impl<N, F, I> Iterator for BreadthFirst<N, F>
where
    N: Clone + Eq + Hash,
    F: FnMut(&N) -> I,
    I: IntoIterator<Item = N>,
{
    type Item = (N, N, usize);

    fn next(&mut self) -> Option<(N, N, usize)> {
        let (from, to, length) = match self.pending.pop_front() {
            None => return None,
            Some(p) => p,
        };
        if self.visited.insert(to.clone()) {
            for neighbor in (self.neighbors)(&to) {
                self.pending.push_back((to.clone(), neighbor, length + 1));
            }
        }
        Some((from, to, length))
    }
}

#[cfg(test)]
mod test {
    use super::*;

    struct EdgeList(Vec<(i32, i32)>);
    impl EdgeList {
        fn neighbors(&self, node: &i32) -> Vec<i32> {
            self.0
                .iter()
                .filter(|(from, _to)| from == node)
                .map(|(_from, to)| *to)
                .collect::<Vec<i32>>()
        }

        fn collect_edges(&self, start: i32) -> Vec<(i32, i32, usize)> {
            breadth_first(start, |n| self.neighbors(n)).collect::<Vec<_>>()
        }
    }

    #[test]
    #[rustfmt::skip]
    fn test_breadth_first() {
        let graph = EdgeList(vec![(2,3), (0,1), (1,2)]);
        assert_eq!(graph.collect_edges(0),
                   vec![(0,1,1), (1,2,2), (2,3,3)]);

        let graph = EdgeList(vec![(0, 1), (1, 10), (0, 2), (2, 10)]);
        assert_eq!(graph.collect_edges(0),
                   vec![(0,1,1), (0,2,1), (1,10,2), (2,10,2)]);

        let graph = EdgeList(vec![(0, 1), (1, 2), (2, 3), (3, 10),
                                  (0, 4), (4, 10),
                                  (0, 5), (5, 10)]);
        assert_eq!(graph.collect_edges(0),
                   vec![(0,1,1), (0,4,1), (0,5,1),
                        (1,2,2), (4,10,2), (5,10,2),
                        (2,3,3),
                        (3,10,4)]);
    }
}

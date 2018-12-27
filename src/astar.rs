use std::cmp::{Ord, Ordering};
use std::collections::{BinaryHeap, HashSet};
use std::fmt::Debug;
use std::hash::Hash;

/// Use the A* algorithm to find shortest paths from `start` to some ending
/// location.
///
/// Return an iterator over all the edges in the graph defined by the
/// `neighbors` function, ordered such that:
///
/// - a node appears as an edge's origin only after it has appeared as a
///   destination (or it is the `start` node), to allow the consumer to actually
///   build paths, and
///
/// - the total length of the final path, as estimated by `neighbors`, is
///   non-decreasing.
///
/// Given any node `n`, the `neighbors` function must return an iterator over
/// pairs `(neighbor, estimate)`, where `neighbor` is every node directly
/// reachable from `n`, and `estimate` is a lower bound on the length of the
/// shortest path from `neighbor` to the ending location. (The estimate is
/// actually the only indication available to the iterator of where the end
/// lies.)
///
/// The iterator `astar` returns favors edges with lower estimated remaining
/// distances, so given meaningful estimates, this algorithm can find a shortest
/// path to `start` exploring many fewer edges than a blind depth-first
/// traversal.
///
/// This iterator eventually produces all edges in the graph; it simply presents
/// them in an order that efficiently finds shortest paths to a given node.
/// If you want to iterate only over edges that are part of shortest paths,
/// use the `shortest_only` adapter.
pub fn astar<N, F, I>(start: N, mut neighbors: F) -> impl Iterator<Item = Edge<N>>
where
    N: Clone + Debug + Eq + Hash,
    F: FnMut(&N) -> I,
    I: IntoIterator<Item = (N, usize)>,
{
    let mut pending = BinaryHeap::new();
    for (neighbor, estimate) in neighbors(&start) {
        pending.push(Edge {
            from: start.clone(),
            to: neighbor,
            path_length: 1,
            estimate,
        });
    }

    let mut visited = HashSet::new();
    visited.insert(start);

    AStar {
        visited,
        pending,
        neighbors,
    }
}

/// An edge in the graph, along with some information about its prospects within
/// the overall traversal.
#[derive(Debug)]
pub struct Edge<N> {
    /// The origin of this edge.
    pub from: N,

    /// The node at which this edge arrives.
    pub to: N,

    /// The number of edges in the shortest path from `start` to `to`.
    pub path_length: usize,

    /// A lower bound on the distance from from this `node` to the `end` node.
    pub estimate: usize,
}

impl<N> Edge<N> {
    fn full_estimate(&self) -> usize {
        self.path_length + self.estimate
    }
}

struct AStar<N, F> {
    visited: HashSet<N>,
    pending: BinaryHeap<Edge<N>>,
    neighbors: F,
}

impl<N, F, I> Iterator for AStar<N, F>
where
    N: Clone + Debug + Eq + Hash,
    F: FnMut(&N) -> I,
    I: IntoIterator<Item = (N, usize)>,
{
    type Item = Edge<N>;

    fn next(&mut self) -> Option<Edge<N>> {
        let edge = match self.pending.pop() {
            None => return None,
            Some(e) => e,
        };
        if self.visited.insert(edge.to.clone()) {
            for (neighbor, estimate) in (self.neighbors)(&edge.to) {
                self.pending.push(Edge {
                    from: edge.to.clone(),
                    to: neighbor,
                    path_length: edge.path_length + 1,
                    estimate,
                });
            }
        }
        Some(edge)
    }
}

// For the sake of `BinaryHeap`, we make `Edge`s ordered with respect to each
// other: A is 'greater than' B if A is a better prospect to consider in the
// traversal - that is, if the estimate of A's path's total length to the `end`
// node (the distance from `start` to `to`, plus the estimate) is less than
// `B`'s.
impl<N> PartialEq for Edge<N> {
    fn eq(&self, other: &Edge<N>) -> bool {
        self.full_estimate() == other.full_estimate()
    }
}

impl<N> Eq for Edge<N> {}

impl<N> Ord for Edge<N> {
    fn cmp(&self, other: &Edge<N>) -> Ordering {
        // If two edges are otherwise equal, prefer the one with the shortest
        // estimate, so we focus our attention on closer edges.
        other
            .full_estimate()
            .cmp(&self.full_estimate())
            .then(other.estimate.cmp(&self.estimate))
    }
}

impl<N> PartialOrd for Edge<N> {
    fn partial_cmp(&self, other: &Edge<N>) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

#[cfg(test)]
mod test {
    use super::*;

    struct EdgeList(Vec<(i32, i32)>);

    impl EdgeList {
        fn neighbors<'a>(&'a self, node: i32) -> impl Iterator<Item = i32> + 'a {
            self.0
                .iter()
                .filter_map(move |(from, to)| if *from == node { Some(*to) } else { None })
        }

        fn collect_astar(&self, start: i32) -> Vec<Edge<i32>> {
            // We have no meaningful way to estimate on an EdgeList, but at
            // least they let us test the basic pathfinding behavior.
            astar(start, |n| self.neighbors(*n).map(|n| (n, 0))).collect::<Vec<_>>()
        }
    }

    fn manhattan(a: &(i32, i32), b: &(i32, i32)) -> usize {
        (i32::abs(a.0 - b.0) + i32::abs(a.1 - b.1)) as usize
    }

    fn von_neumann_neighbors(
        node: (i32, i32),
        end: (i32, i32),
    ) -> impl Iterator<Item = ((i32, i32), usize)> {
        const DIRS: [(i32, i32); 4] = [(1, 0), (0, 1), (-1, 0), (0, -1)];
        DIRS.iter()
            .map(move |(dx, dy)| (node.0 + dx, node.1 + dy))
            .map(move |pt| (pt, manhattan(&pt, &end)))
    }

    fn collect_manhattan_astar(
        start: (i32, i32),
        end: (i32, i32),
        limit: usize,
    ) -> Vec<Edge<(i32, i32)>> {
        astar(start, |n| von_neumann_neighbors(*n, end))
            .take(limit)
            .collect::<Vec<_>>()
    }

    #[test]
    #[rustfmt::skip]
    fn test_astar() {
        let graph = EdgeList(vec![(2,3), (0,1), (1,2)]);
        assert_eq!(graph.collect_astar(0),
                   vec![Edge { from: 0, to: 1, path_length: 1, estimate: 0 },
                        Edge { from: 1, to: 2, path_length: 2, estimate: 0 },
                        Edge { from: 2, to: 3, path_length: 3, estimate: 0 }]);

        let graph = EdgeList(vec![(0, 1), (1, 10), (0, 2), (2, 10)]);
        assert_eq!(graph.collect_astar(0),
                   vec![Edge { from: 0, to: 1, path_length: 1, estimate: 0 },
                        Edge { from: 0, to: 2, path_length: 1, estimate: 0 },
                        Edge { from: 1, to: 10, path_length: 2, estimate: 0 },
                        Edge { from: 2, to: 10, path_length: 2, estimate: 0 }]);

        let graph = EdgeList(vec![(0, 1), (1, 2), (2, 3), (3, 10),
                                  (0, 4), (4, 10),
                                  (0, 5), (5, 10)]);
        assert_eq!(graph.collect_astar(0),
                   vec![Edge { from: 0, to: 1, path_length: 1, estimate: 0 },
                        Edge { from: 0, to: 4, path_length: 1, estimate: 0 },
                        Edge { from: 0, to: 5, path_length: 1, estimate: 0 },
                        Edge { from: 1, to: 2, path_length: 2, estimate: 0 },
                        Edge { from: 4, to: 10, path_length: 2, estimate: 0 },
                        Edge { from: 5, to: 10, path_length: 2, estimate: 0 },
                        Edge { from: 2, to: 3, path_length: 3, estimate: 0 },
                        Edge { from: 3, to: 10, path_length: 4, estimate: 0 }]);

        // We expect this to produce every edge heading in the right direction
        // that lies within the rectangle bounded by the start and the end,
        // before it produces anything outside the rectangle or going in the
        // wrong direction.
        assert_eq!(collect_manhattan_astar((0, 0), (3,3), 3*4 + 3*4 + 1),
                   vec![
                       // along the bottom edge
                       Edge { from: (0, 0), to: (1, 0), path_length: 1, estimate: 5 },
                       Edge { from: (1, 0), to: (2, 0), path_length: 2, estimate: 4 },
                       // up the side
                       Edge { from: (2, 0), to: (3, 0), path_length: 3, estimate: 3 },
                       Edge { from: (3, 0), to: (3, 1), path_length: 4, estimate: 2 },
                       Edge { from: (3, 1), to: (3, 2), path_length: 5, estimate: 1 },
                       // victory
                       Edge { from: (3, 2), to: (3, 3), path_length: 6, estimate: 0 },
                       // now must arrive by the other edge, so leave x axis
                       // one step earlier, and hit all the edges that are at
                       // least going in the right direction.
                       Edge { from: (2, 0), to: (2, 1), path_length: 3, estimate: 3 },
                       Edge { from: (2, 1), to: (3, 1), path_length: 4, estimate: 2 },
                       Edge { from: (2, 1), to: (2, 2), path_length: 4, estimate: 2 },
                       Edge { from: (2, 2), to: (3, 2), path_length: 5, estimate: 1 },
                       Edge { from: (2, 2), to: (2, 3), path_length: 5, estimate: 1 },
                       Edge { from: (2, 3), to: (3, 3), path_length: 6, estimate: 0 },
                       // fill in the rest of the rectangle
                       Edge { from: (1, 0), to: (1, 1), path_length: 2, estimate: 4 },
                       Edge { from: (1, 1), to: (2, 1), path_length: 3, estimate: 3 },
                       Edge { from: (1, 1), to: (1, 2), path_length: 3, estimate: 3 },
                       Edge { from: (1, 2), to: (2, 2), path_length: 4, estimate: 2 },
                       Edge { from: (1, 2), to: (1, 3), path_length: 4, estimate: 2 },
                       Edge { from: (1, 3), to: (2, 3), path_length: 5, estimate: 1 },
                       Edge { from: (0, 0), to: (0, 1), path_length: 1, estimate: 5 },
                       Edge { from: (0, 1), to: (1, 1), path_length: 2, estimate: 4 },
                       Edge { from: (0, 1), to: (0, 2), path_length: 2, estimate: 4 },
                       Edge { from: (0, 2), to: (1, 2), path_length: 3, estimate: 3 },
                       Edge { from: (0, 2), to: (0, 3), path_length: 3, estimate: 3 },
                       Edge { from: (0, 3), to: (1, 3), path_length: 4, estimate: 2 },
                       // finally, our first really wrong edge, heading in the
                       // wrong direction
                       Edge { from: (3, 3), to: (3, 2), path_length: 7, estimate: 1 }]);
    }
}

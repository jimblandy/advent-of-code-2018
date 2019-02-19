use std::cmp::{Ord, Ordering};
use std::collections::{BinaryHeap, HashSet};
use std::fmt::Debug;
use std::hash::Hash;
use std::ops::Add;

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
/// pairs `(neighbor, weight, estimate)`, where `neighbor` is a node directly
/// reachable from `n`, `weight` is the cost of the edge from `n` to that
/// neighbor, and `estimate` is a lower bound on the total weight of the
/// shortest path from `neighbor` to the ending location. (The estimate is
/// actually the only indication available to `astar_weighted` of where the end
/// lies.)
///
/// You may use any type you like for edge weights, as long as it can be added,
/// ordered, and cloned. If all edges have the same weight, it may be simpler to
/// use `astar`.
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
pub fn astar_weighted<N, F, I, W>(start: N, mut neighbors: F) -> AStarWeighted<N, F, W>
where
    N: Clone + Debug + Eq + Hash,
    F: FnMut(&N) -> I,
    I: IntoIterator<Item = (N, W, W)>,
    W: Add<Output = W> + Clone + Ord,
{
    let mut pending = BinaryHeap::new();
    for (neighbor, weight, estimate) in neighbors(&start) {
        pending.push(Edge {
            from: start.clone(),
            to: neighbor,
            path_weight: weight,
            estimate: estimate,
        });
    }

    let mut visited = HashSet::new();
    visited.insert(start);

    AStarWeighted {
        visited,
        pending,
        neighbors,
    }
}

/// An edge in the graph, along with some information about its prospects within
/// the overall traversal.
#[derive(Debug)]
pub struct Edge<N, W> {
    /// The origin of this edge.
    pub from: N,

    /// The node at which this edge arrives.
    pub to: N,

    /// The total weight of the edges in the shortest path from `start` to `to`.
    pub path_weight: W,

    /// A lower bound on the weight of the path from this `node` to the `end` node.
    pub estimate: W,
}

impl<N, W> Edge<N, W>
where
    W: Add<Output = W> + Ord + Clone,
{
    fn full_estimate(&self) -> W {
        self.path_weight.clone() + self.estimate.clone()
    }
}

pub struct AStarWeighted<N, F, W> {
    visited: HashSet<N>,
    pending: BinaryHeap<Edge<N, W>>,
    neighbors: F,
}

impl<N, F, I, W> Iterator for AStarWeighted<N, F, W>
where
    N: Clone + Debug + Eq + Hash,
    F: FnMut(&N) -> I,
    I: IntoIterator<Item = (N, W, W)>,
    W: Add<Output = W> + Clone + Ord,
{
    type Item = Edge<N, W>;

    fn next(&mut self) -> Option<Edge<N, W>> {
        let edge = match self.pending.pop() {
            None => return None,
            Some(e) => e,
        };
        if self.visited.insert(edge.to.clone()) {
            for (neighbor, weight, estimate) in (self.neighbors)(&edge.to) {
                self.pending.push(Edge {
                    from: edge.to.clone(),
                    to: neighbor,
                    path_weight: edge.path_weight.clone() + weight,
                    estimate,
                });
            }
        }
        Some(edge)
    }
}

// For the sake of `BinaryHeap`, we make `Edge`s ordered with respect to each
// other: A is 'greater than' B if A's total weight is *shorter* than B's - that
// is, if A should be considered before B.
impl<N, W> PartialEq for Edge<N, W>
where
    W: Add<Output = W> + Ord + Clone,
{
    fn eq(&self, other: &Edge<N, W>) -> bool {
        self.full_estimate() == other.full_estimate()
    }
}

impl<N, W> Eq for Edge<N, W> where W: Add<Output = W> + Ord + Clone {}

impl<N, W> Ord for Edge<N, W>
where
    W: Add<Output = W> + Ord + Clone,
{
    fn cmp(&self, other: &Edge<N, W>) -> Ordering {
        // If two edges are otherwise equal, prefer the one with the shortest
        // estimate, so we focus our attention on closer edges.
        other
            .full_estimate()
            .cmp(&self.full_estimate())
            .then(other.estimate.cmp(&self.estimate))
    }
}

impl<N, W> PartialOrd for Edge<N, W>
where
    W: Add<Output = W> + Ord + Clone,
{
    fn partial_cmp(&self, other: &Edge<N, W>) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

#[cfg(test)]
mod test {
    use super::*;

    struct EdgeList(Vec<(i32, i32, i32)>);

    impl EdgeList {
        fn neighbors<'a>(&'a self, node: i32) -> impl Iterator<Item = (i32, i32, i32)> + 'a {
            self.0.iter().filter_map(move |(from, to, weight)| {
                if *from == node {
                    Some((*to, *weight, 0))
                } else {
                    None
                }
            })
        }

        fn collect_astarw(&self, start: i32) -> Vec<Edge<i32, i32>> {
            astar_weighted(start, |n| self.neighbors(*n)).collect::<Vec<_>>()
        }
    }

    #[test]
    #[rustfmt::skip]
    fn test_bfw() {
        let graph = EdgeList(vec![(0,1,2), (0,1,3), (0,1,1)]);
        assert_eq!(graph.collect_astarw(0),
                   vec![Edge { from: 0, to: 1, path_weight: 1, estimate: 0 },
                        Edge { from: 0, to: 1, path_weight: 2, estimate: 0 },
                        Edge { from: 0, to: 1, path_weight: 3, estimate: 0 }]);

        let graph = EdgeList(vec![(0, 1, 2), (1, 3, 1),
                                  (0, 2, 1), (2, 3, 3)]);
        assert_eq!(graph.collect_astarw(0),
                   vec![Edge { from: 0, to: 2, path_weight: 1, estimate: 0 },
                        Edge { from: 0, to: 1, path_weight: 2, estimate: 0 },
                        Edge { from: 1, to: 3, path_weight: 3, estimate: 0 },
                        Edge { from: 2, to: 3, path_weight: 4, estimate: 0 }]);

        let graph = EdgeList(vec![(0, 1, 2), (1, 2, 2), (2, 3, 1), (3, 10, 1),
                                  (0, 4, 1), (4, 10, 7),
                                  (0, 5, 3), (5, 10, 4)]);
        assert_eq!(graph.collect_astarw(0),
                   vec![Edge { from: 0, to: 4,  path_weight: 1, estimate: 0 },
                        Edge { from: 0, to: 1,  path_weight: 2, estimate: 0 },
                        Edge { from: 0, to: 5,  path_weight: 3, estimate: 0 },
                        Edge { from: 1, to: 2,  path_weight: 4, estimate: 0 },
                        Edge { from: 2, to: 3,  path_weight: 5, estimate: 0 },
                        Edge { from: 3, to: 10, path_weight: 6, estimate: 0 },
                        Edge { from: 5, to: 10, path_weight: 7, estimate: 0 },
                        Edge { from: 4, to: 10, path_weight: 8, estimate: 0 }]);
    }
}

use std::collections::{HashSet, VecDeque};
use std::hash::Hash;

/// Iterate over the edges of a graph in depth-first order.
///
/// Starting at `start`, and finding nodes' neighbors by calling `neighbors`,
/// produce every edge in the graph as a triple `(from, to, length)`, where
/// `length` is the total length of a shortest path from `start` to `to`, in
/// order of non-decreasing `length`.
pub fn shortest_paths<N, F, I>(start: N, mut neighbors: F) -> impl Iterator<Item=(N, N, usize)>
where N: Clone + Eq + Hash,
      F: FnMut(&N) -> I,
      I: IntoIterator<Item=N>,
{
    let mut pending = VecDeque::new();
    for neighbor in neighbors(&start) {
        pending.push_back((start.clone(), neighbor, 1));
    }

    let mut visited = HashSet::new();
    visited.insert(start);

    ShortestPaths { visited, pending, neighbors }
}

struct ShortestPaths<N, F> {
    visited: HashSet<N>,
    pending: VecDeque<(N, N, usize)>,
    neighbors: F,
}

impl<N, F, I> Iterator for ShortestPaths<N, F>
where N: Clone + Eq + Hash,
      F: FnMut(&N) -> I,
      I: IntoIterator<Item=N>,
{
    type Item=(N, N, usize);

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
            self.0.iter()
                .filter(|(from, _to)| from == node)
                .map(|(_from, to)| *to)
                .collect::<Vec<i32>>()
        }

        fn collect_edges(&self, start: i32) -> Vec<(i32, i32, usize)> {
            shortest_paths(start, |n| self.neighbors(n)).collect::<Vec<_>>()
        }
    }

    #[test]
    fn test_shortest_paths() {
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


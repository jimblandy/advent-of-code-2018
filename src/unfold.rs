/// Return an iterator producing the sequence of values returned by repeated
/// applications of `step` to a state value.
///
/// The `step` closure takes a state value and returns either `Some((next_state,
/// item))`, where `item` is the value for the iterator to produce and
/// `next_state` is the value to pass to `step` on the next iteration; or `None`
/// to indicate that iteration is over.
///
/// For example:
///
///     # extern crate advent_of_code_2018 as aoc;
///     # use aoc::unfold::unfold;
///     let fib = unfold((0, 1), |(a, b)| Some(((b, a+b), b)));
///     assert_eq!(fib.take(5).collect::<Vec<_>>(), vec![1,1,2,3,5]);
pub fn unfold<T, U, F>(initial: T, step: F) -> impl Iterator<Item = U>
where
    F: FnMut(T) -> Option<(T, U)>,
{
    Unfolder(Some((step, initial)))
}

#[derive(Debug, Clone)]
pub struct Unfolder<F, T>(Option<(F, T)>);

impl<T, U, F> Iterator for Unfolder<F, T>
where
    F: FnMut(T) -> Option<(T, U)>,
{
    type Item = U;
    fn next(&mut self) -> Option<U> {
        match self.0.take() {
            None => None,
            Some((mut step, state)) => match step(state) {
                None => None,
                Some((next_state, item)) => {
                    self.0 = Some((step, next_state));
                    Some(item)
                }
            },
        }
    }
}

#[test]
fn test_unfold() {
    let fib = unfold((0, 1), |(a, b)| Some(((b, a + b), b)));
    assert_eq!(
        fib.take(10).collect::<Vec<_>>(),
        vec![1, 1, 2, 3, 5, 8, 13, 21, 34, 55]
    );
}

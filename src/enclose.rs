use std::cmp::{max, min};
use std::ops::{Range, RangeFrom, RangeFull, RangeInclusive, RangeTo, RangeToInclusive};

/// An analog to a `union` operation for ranges.
///
/// In general, the union of two ranges is not itself a range: it might have a
/// gap. But we can at least find the smallest range that encloses two ranges.
///
/// We don't provide implementations that mix end-inclusive and end-exclusive
/// ranges. We can't actually convert one kind of boundary to the other, but the
/// choice of one or the other may be dynamic.
///
/// We do provide implementation for `RangeFull`, even though the output type is
/// always `RangeFull`. These are a little silly, but they might be useful for
/// functions that are generic over `T: Enclose<U>`.
pub trait Enclose<Right> {
    type Output;

    /// Return the smallest range that encloses both `self` and `right`.
    fn enclose(self, right: Right) -> Self::Output;
}

macro_rules! impl_enclose {
    ( <$tyvar:ident>($sylf:ident, $right:ident):
       $( for $Left:ty {
           $( ($Right:ty) -> $output:ty {
               $body:expr
           } )*
       } )*
    ) => {
        $(
            $(
                impl<$tyvar: Ord> Enclose<$Right> for $Left {
                    type Output = $output;
                    #[allow(unused_variables)]
                    fn enclose($sylf, $right: $Right) -> $output {
                        $body
                    }
                }
            )*
        )*
    }
}

impl_enclose! {
    <Idx>(self, right):

    for RangeFrom<Idx> {
        (RangeFull) -> RangeFull { .. }
        (RangeFrom<Idx>) -> RangeFrom<Idx> { min(self.start, right.start).. }
        (RangeTo<Idx>) -> RangeFull { .. }
        (RangeToInclusive<Idx>) -> RangeFull { .. }
        (Range<Idx>) -> RangeFrom<Idx> { min(self.start, right.start) .. }
        (RangeInclusive<Idx>) -> RangeFrom<Idx> {
            {
                let (start, end) = right.into_inner();
                min(self.start, start) ..
            }
        }
    }

    for RangeTo<Idx> {
        (RangeFull) -> RangeFull { .. }
        (RangeFrom<Idx>) -> RangeFull { .. }
        (RangeTo<Idx>) -> RangeTo<Idx> { .. max(self.end, right.end) }
     // (RangeToInclusive<Idx>) cannot mix inclusive/exclusive
        (Range<Idx>) -> RangeTo<Idx> { .. max(self.end, right.end) }
     // (RangeInclusive<Idx>) cannot mix inclusive/exclusive
    }

    for RangeToInclusive<Idx> {
        (RangeFull) -> RangeFull { .. }
        (RangeFrom<Idx>) -> RangeFull { .. }
     // (RangeTo<Idx>) cannot mix inclusive/exclusive
        (RangeToInclusive<Idx>) -> RangeToInclusive<Idx> { ..= max(self.end, right.end) }
     // (Range<Idx>) cannot mix inclusive/exclusive
        (RangeInclusive<Idx>) -> RangeToInclusive<Idx> {
            {
                let (start, end) = right.into_inner();
                ..= max(self.end, end)
            }
        }
    }

    for Range<Idx> {
        (RangeFull) -> RangeFull { .. }
        (RangeFrom<Idx>) -> RangeFrom<Idx> { min(self.start, right.start) .. }
        (RangeTo<Idx>) -> RangeTo<Idx> { .. max(self.end, right.end) }
     // (RangeToInclusive<Idx>) cannot mix inclusive/exclusive
        (Range<Idx>) -> Range<Idx> { min(self.start, right.start) .. max(self.end, right.end) }
     // (RangeInclusive<Idx>) cannot mix inclusive/exclusive
    }

    for RangeInclusive<Idx> {
        (RangeFull) -> RangeFull { .. }
        (RangeFrom<Idx>) -> RangeFrom<Idx> {
            {
                let (start, end) = self.into_inner();
                min(start, right.start) ..
            }
        }
     // (RangeTo<Idx>) cannot mix inclusive/exclusive
        (RangeToInclusive<Idx>) -> RangeToInclusive<Idx> {
            {
                let (start, end) = self.into_inner();
                ..= max(end, right.end)
            }
        }
     // (Range<Idx>) cannot mix inclusive/exclusive
        (RangeInclusive<Idx>) -> RangeInclusive<Idx> {
            {
                let (left_start, left_end) = self.into_inner();
                let (right_start, right_end) = right.into_inner();
                min(left_start, right_start) ..= max(left_end, right_end)
            }
        }
    }
}

impl<Right> Enclose<Right> for RangeFull {
    type Output = RangeFull;
    fn enclose(self, _right: Right) -> RangeFull {
        ..
    }
}

#[test]
fn test_enclose() {
    assert_eq!((..).enclose(..), ..);
    assert_eq!((..).enclose(1..), ..);
    assert_eq!((..).enclose(..1), ..);
    assert_eq!((..).enclose(..=1), ..);
    assert_eq!((..).enclose(1..2), ..);
    assert_eq!((..).enclose(1..=2), ..);

    assert_eq!((0..).enclose(..), ..);
    assert_eq!((0..).enclose(1..), 0..);
    assert_eq!((1..).enclose(0..), 0..);
    assert_eq!((0..).enclose(..1), ..);
    assert_eq!((0..).enclose(..=1), ..);
    assert_eq!((0..).enclose(1..2), 0..);
    assert_eq!((1..).enclose(0..2), 0..);
    assert_eq!((0..).enclose(1..=2), 0..);
    assert_eq!((1..).enclose(0..=2), 0..);

    assert_eq!((..1).enclose(..), ..);
    assert_eq!((..1).enclose(0..), ..);
    assert_eq!((..1).enclose(..2), ..2);
    assert_eq!((..1).enclose(..0), ..1);
    assert_eq!((..2).enclose(0..3), ..3);
    assert_eq!((..2).enclose(0..1), ..2);

    assert_eq!((..=1).enclose(..), ..);
    assert_eq!((..=1).enclose(0..), ..);
    assert_eq!((..=1).enclose(..=2), ..=2);
    assert_eq!((..=1).enclose(..=0), ..=1);
    assert_eq!((..=2).enclose(0..=3), ..=3);
    assert_eq!((..=2).enclose(0..=1), ..=2);

    assert_eq!((1..3).enclose(..), ..);
    assert_eq!((1..3).enclose(0..), 0..);
    assert_eq!((1..3).enclose(2..), 1..);
    assert_eq!((1..3).enclose(..4), ..4);
    assert_eq!((1..3).enclose(..2), ..3);
    assert_eq!((1..3).enclose(0..1), 0..3);
    assert_eq!((1..3).enclose(0..2), 0..3);
    assert_eq!((1..3).enclose(0..4), 0..4);
    assert_eq!((1..3).enclose(2..2), 1..3);
    assert_eq!((1..3).enclose(2..4), 1..4);
    assert_eq!((1..3).enclose(4..5), 1..5);
}

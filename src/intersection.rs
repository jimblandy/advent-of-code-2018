use std::cmp::{max, min};
use std::ops::{Range, RangeFrom, RangeFull, RangeInclusive, RangeTo, RangeToInclusive};

pub trait Intersection<Right> {
    type Output;
    fn intersection(self, right: Right) -> Self::Output;
}

macro_rules! impl_intersection {
    ( <$tyvar:ident>($sylf:ident, $right:ident):
       $( for $Left:ty {
           $( ($Right:ty) -> $output:ty {
               $body:expr
           } )*
       } )*
    ) => {
        $(
            $(
                impl<$tyvar: Ord> Intersection<$Right> for $Left {
                    type Output = $output;
                    #[allow(unused_variables)]
                    fn intersection($sylf, $right: $Right) -> $output {
                        $body
                    }
                }
            )*
        )*
    }
}

impl_intersection! {
    <Idx>(self, right):

    for RangeFrom<Idx> {
        (RangeFull) -> RangeFrom<Idx> { self }
        (RangeFrom<Idx>) -> RangeFrom<Idx> { max(self.start, right.start).. }
        (RangeTo<Idx>) -> Range<Idx> { self.start .. right.end }
        (RangeToInclusive<Idx>) -> RangeInclusive<Idx> { self.start ..= right.end }
        (Range<Idx>) -> Range<Idx> { max(self.start, right.start) .. right.end }
        (RangeInclusive<Idx>) -> RangeInclusive<Idx> {
            {
                let (start, end) = right.into_inner();
                max(self.start, start) ..= end
            }
        }
    }

    for RangeTo<Idx> {
        (RangeFull) -> RangeTo<Idx> { self }
        (RangeFrom<Idx>) -> Range<Idx> { right.start .. self.end }
        (RangeTo<Idx>) -> RangeTo<Idx> { .. min(self.end, right.end) }
     // (RangeToInclusive<Idx>) cannot mix inclusive/exclusive
        (Range<Idx>) -> Range<Idx> { right.start .. min(self.end, right.end) }
     // (RangeInclusive<Idx>) cannot mix inclusive/exclusive
    }

    for RangeToInclusive<Idx> {
        (RangeFull) -> RangeToInclusive<Idx> { self }
        (RangeFrom<Idx>) -> RangeInclusive<Idx> { right.start ..= self.end }
     // (RangeTo<Idx>) cannot mix inclusive/exclusive
        (RangeToInclusive<Idx>) -> RangeToInclusive<Idx> { ..= min(self.end, right.end) }
     // (Range<Idx>) cannot mix inclusive/exclusive
        (RangeInclusive<Idx>) -> RangeInclusive<Idx> {
            {
                let (start, end) = right.into_inner();
                start ..= min(self.end, end)
            }
        }
    }

    for Range<Idx> {
        (RangeFull) -> Range<Idx> { self }
        (RangeFrom<Idx>) -> Range<Idx> { max(self.start, right.start) .. self.end }
        (RangeTo<Idx>) -> Range<Idx> { self.start .. min(self.end, right.end) }
     // (RangeToInclusive<Idx>) cannot mix inclusive/exclusive
        (Range<Idx>) -> Range<Idx> { max(self.start, right.start) .. min(self.end, right.end) }
     // (RangeInclusive<Idx>) cannot mix inclusive/exclusive
    }

    for RangeInclusive<Idx> {
        (RangeFull) -> RangeInclusive<Idx> { self }
        (RangeFrom<Idx>) -> RangeInclusive<Idx> {
            {
                let (start, end) = self.into_inner();
                max(start, right.start) ..= end
            }
        }
     // (RangeTo<Idx>) cannot mix inclusive/exclusive
        (RangeToInclusive<Idx>) -> RangeInclusive<Idx> {
            {
                let (start, end) = self.into_inner();
                start ..= min(end, right.end)
            }
        }
     // (Range<Idx>) cannot mix inclusive/exclusive
        (RangeInclusive<Idx>) -> RangeInclusive<Idx> {
            {
                let (left_start, left_end) = self.into_inner();
                let (right_start, right_end) = right.into_inner();
                max(left_start, right_start) ..= min(left_end, right_end)
            }
        }
    }
}

impl<Right> Intersection<Right> for RangeFull {
    type Output = Right;
    fn intersection(self, right: Right) -> Self::Output {
        right
    }
}

#[test]
fn test_intersection() {
    assert_eq!((..).intersection(..), ..);
    assert_eq!((..).intersection(1..), 1..);
    assert_eq!((..).intersection(..1), ..1);
    assert_eq!((..).intersection(..=1), ..=1);
    assert_eq!((..).intersection(1..2), 1..2);
    assert_eq!((..).intersection(1..=2), 1..=2);

    assert_eq!((0..).intersection(..), 0..);
    assert_eq!((0..).intersection(1..), 1..);
    assert_eq!((1..).intersection(0..), 1..);
    assert_eq!((0..).intersection(..1), 0..1);
    assert_eq!((0..).intersection(..=1), 0..=1);
    assert_eq!((0..).intersection(1..2), 1..2);
    assert_eq!((1..).intersection(0..2), 1..2);
    assert_eq!((0..).intersection(1..=2), 1..=2);
    assert_eq!((1..).intersection(0..=2), 1..=2);

    assert_eq!((..1).intersection(..), ..1);
    assert_eq!((..1).intersection(0..), 0..1);
    assert_eq!((..1).intersection(..2), ..1);
    assert_eq!((..1).intersection(..0), ..0);
    assert_eq!((..2).intersection(0..3), 0..2);
    assert_eq!((..2).intersection(0..1), 0..1);

    assert_eq!((..=1).intersection(..), ..=1);
    assert_eq!((..=1).intersection(0..), 0..=1);
    assert_eq!((..=1).intersection(..=2), ..=1);
    assert_eq!((..=1).intersection(..=0), ..=0);
    assert_eq!((..=2).intersection(0..=3), 0..=2);
    assert_eq!((..=2).intersection(0..=1), 0..=1);

    assert_eq!((1..3).intersection(..), 1..3);
    assert_eq!((1..3).intersection(0..), 1..3);
    assert_eq!((1..3).intersection(2..), 2..3);
    assert_eq!((1..3).intersection(..4), 1..3);
    assert_eq!((1..3).intersection(..2), 1..2);
    assert_eq!((1..3).intersection(0..1), 1..1);
    assert_eq!((1..3).intersection(0..2), 1..2);
    assert_eq!((1..3).intersection(0..4), 1..3);
    assert!(Range::is_empty(&(1..3).intersection(2..2)));
    assert_eq!((1..3).intersection(2..4), 2..3);
    assert!(Range::is_empty(&(1..3).intersection(4..5)));
}

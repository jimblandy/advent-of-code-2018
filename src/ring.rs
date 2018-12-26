//! A `Ring<T>` is a non-empty doubly-linked list of `T` values.

use std::fmt;
use std::marker::PhantomData;
use std::ptr::{null, null_mut};

/// A ring's elements always form a cycle: the last element's `next` points to
/// the first element, and vice versa.
struct Element<T> {
    next: *mut Element<T>,
    prev: *mut Element<T>,
    value: T,
}

impl<T> Element<T> {
    /// Splice the ring `other` in just before `self`. Both must be well-formed
    /// elements of a ring.
    fn splice_before(&mut self, other: &mut Element<T>) {
        let last = unsafe { &mut *self.prev };
        let other_last = unsafe { &mut *other.prev };

        self.prev = other_last;
        other_last.next = self;

        last.next = other;
        other.prev = last;
    }

    fn is_singleton(&self) -> bool {
        self.next as *const _ == self as *const _
    }
}

pub struct Ring<T>(*mut Element<T>);

impl<T> Ring<T> {
    fn into_front(self) -> *mut Element<T> {
        let raw = self.0;
        std::mem::forget(self);
        raw
    }
}

impl<'a, T> IntoIterator for &'a Ring<T> {
    type Item = &'a T;
    type IntoIter = Iter<'a, T>;
    fn into_iter(self) -> Iter<'a, T> {
        Iter {
            next: self.0,
            head: self.0,
            lifetime_like: PhantomData,
        }
    }
}

pub struct Iter<'a, T> {
    next: *const Element<T>,
    head: *const Element<T>,
    lifetime_like: PhantomData<&'a T>,
}

impl<'a, T> Iterator for Iter<'a, T> {
    type Item = &'a T;
    fn next(&mut self) -> Option<&'a T> {
        if self.next.is_null() {
            None
        } else {
            let elt: &'a Element<T> = unsafe { &*self.next };
            self.next = elt.next;
            if self.next == self.head {
                self.next = null();
            }
            Some(&elt.value)
        }
    }
}

impl<'a, T> IntoIterator for &'a mut Ring<T> {
    type Item = &'a mut T;
    type IntoIter = IterMut<'a, T>;
    fn into_iter(self) -> IterMut<'a, T> {
        IterMut {
            next: self.0,
            head: self.0,
            lifetime_like: PhantomData,
        }
    }
}

pub struct IterMut<'a, T> {
    next: *mut Element<T>,
    head: *mut Element<T>,
    lifetime_like: PhantomData<&'a mut T>,
}

impl<'a, T> Iterator for IterMut<'a, T> {
    type Item = &'a mut T;
    fn next(&mut self) -> Option<&'a mut T> {
        if self.next.is_null() {
            return None;
        }

        let elt: &'a mut Element<T> = unsafe { &mut *self.next };
        self.next = if elt.next != self.head {
            elt.next
        } else {
            null_mut()
        };

        Some(&mut elt.value)
    }
}

impl<T> Drop for Ring<T> {
    fn drop(&mut self) {
        let mut next = self.0;
        loop {
            let mut here = unsafe { Box::from_raw(next) };

            next = here.next;
            here.next = null_mut();
            here.prev = null_mut();
            if next == self.0 {
                break;
            }
        }
        self.0 = null_mut();
    }
}

impl<T> Ring<T> {
    pub fn new(value: T) -> Ring<T> {
        let elt = Box::new(Element {
            next: null_mut(),
            prev: null_mut(),
            value,
        });
        let raw = Box::into_raw(elt);
        unsafe {
            (*raw).next = raw;
            (*raw).prev = raw;
        }
        Ring(raw)
    }

    pub fn front(&self) -> &T {
        unsafe { &(*self.0).value }
    }

    pub fn rotate_forward(&mut self, n: usize) {
        let mut elt = self.0;
        for _ in 0..n {
            elt = unsafe { (*elt).next };
        }
        self.0 = elt;
    }

    pub fn rotate_backward(&mut self, n: usize) {
        let mut elt = self.0;
        for _ in 0..n {
            elt = unsafe { (*elt).prev };
        }
        self.0 = elt;
    }

    pub fn splice_at_front(&mut self, other: Ring<T>) {
        let front = unsafe { &mut *self.0 };
        let other = unsafe { &mut *other.into_front() };
        front.splice_before(other);
        self.0 = other;
    }

    pub fn insert_at_front(&mut self, value: T) {
        self.splice_at_front(Ring::new(value))
    }

    // Pop the front element of `self`. Return the remaining ring, if it has any
    // other elements.
    pub fn pop_front(self) -> (T, Option<Ring<T>>) {
        unsafe {
            let front = Box::from_raw(self.into_front());
            let remainder = if front.is_singleton() {
                None
            } else {
                (*front.next).prev = front.prev;
                (*front.prev).next = front.next;
                Some(Ring(front.next))
            };
            let value = front.value; // move out of box
            (value, remainder)
        }
    }
}

impl<T: fmt::Debug> fmt::Debug for Ring<T> {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        fmt.debug_list().entries(self).finish()
    }
}

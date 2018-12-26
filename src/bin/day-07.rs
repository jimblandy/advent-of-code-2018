use std::collections::{BTreeMap, HashMap};

#[allow(dead_code)]
static TEST_INPUT: &'static str = include_str!("day-07.test");
#[allow(dead_code)]
static INPUT: &'static str = include_str!("day-07.input");

#[derive(Debug, Eq, PartialEq)]
struct Event<T> {
    time: usize,
    value: T,
}

#[derive(Default)]
struct TimedQueue<T>(Vec<Event<T>>); // sorted by decreasing time

impl<T> TimedQueue<T> {
    fn push(&mut self, event: Event<T>) {
        let insert_at = (0..self.0.len())
            .find(|&i| self.0[i].time < event.time)
            .unwrap_or(self.0.len());
        self.0.insert(insert_at, event)
    }

    fn pop(&mut self) -> Option<Event<T>> {
        self.0.pop()
    }

    fn len(&self) -> usize {
        self.0.len()
    }
}

#[test]
fn test_queue() {
    let mut q = TimedQueue::default();
    q.push(Event {
        time: 10,
        value: 'a',
    });
    q.push(Event {
        time: 5,
        value: 'b',
    });
    q.push(Event {
        time: 15,
        value: 'c',
    });
    q.push(Event {
        time: 10,
        value: 'd',
    });

    assert_eq!(
        q.pop(),
        Some(Event {
            time: 5,
            value: 'b'
        })
    );
    assert_eq!(
        q.pop(),
        Some(Event {
            time: 10,
            value: 'd'
        })
    );
    assert_eq!(
        q.pop(),
        Some(Event {
            time: 10,
            value: 'a'
        })
    );
    assert_eq!(
        q.pop(),
        Some(Event {
            time: 15,
            value: 'c'
        })
    );
}

fn task_time(task: char) -> usize {
    61 + (task as usize - 'A' as usize)
}

fn main() {
    let deps: Vec<(char, char)> = INPUT
        .lines()
        .map(|line| {
            assert_eq!(&line[..5], "Step ");
            assert_eq!(&line[6..36], " must be finished before step ");
            assert_eq!(&line[37..], " can begin.");

            (
                line[5..].chars().next().unwrap(),
                line[36..].chars().next().unwrap(),
            )
        })
        .collect();

    let mut posts: HashMap<char, Vec<char>> = HashMap::new();
    let mut blockers: BTreeMap<char, usize> = BTreeMap::new();
    for (pre, post) in &deps {
        posts.entry(*post).or_insert(vec![]);
        posts.entry(*pre).or_insert(vec![]).push(*post);
        blockers.entry(*pre).or_insert(0);
        *blockers.entry(*post).or_insert(0) += 1;
    }

    let mut sequence = String::new();
    let mut now = 0;
    const TOTAL_WORKERS: usize = 5;
    let mut busy_workers = 0;
    let mut pending = TimedQueue::default();
    loop {
        // There ought to be an entry in `pending` for each busy worker.
        assert_eq!(busy_workers, pending.len());

        // Start as many workers as we can.
        while busy_workers < TOTAL_WORKERS {
            if let Some((&next, _)) = blockers.iter().find(|&(_i, &n)| n == 0) {
                blockers.remove(&next);
                busy_workers += 1;
                pending.push(Event {
                    time: now + task_time(next),
                    value: next,
                });
            } else {
                break;
            }
        }

        println!("Active tasks at time {}:", now);
        for event in &pending.0 {
            println!("  {}, done {}", event.value, event.time);
        }
        println!();

        // Still.
        assert_eq!(busy_workers, pending.len());

        // If no workers are busy, then all tasks must be complete.
        if busy_workers == 0 {
            break;
        }

        // We know we're running everything we can, so now we have to wait for
        // something to be finished. Since some workers are busy, there must be
        // something in the queue.
        let finished = pending.pop().unwrap();
        busy_workers -= 1;
        now = finished.time;

        // Record the task we just finished.
        sequence.push(finished.value);

        // Remove the completed task as a blocker for anything else.
        for post in &posts[&finished.value] {
            *blockers.get_mut(&post).unwrap() -= 1;
        }
    }

    println!("Final time: {}", now);

    println!("Sequence: {}", sequence);
    println!("Remaining counts: {:#?}", blockers);
}

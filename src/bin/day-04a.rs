use std::collections::HashMap;
use std::io::BufRead;
use std::ops::Range;
use std::str::FromStr;

const MONTH_START_DAY: [usize; 12] = [0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334];

/// Parse `t` as minutes past 1518-1-1 00:00
fn parse_time(t: &str) -> usize {
    assert_eq!(&t[0..6], "[1518-");
    assert_eq!(&t[17..18], "]");
    let fields: Vec<usize> = t[6..17]
        .split(&['-', ' ', ':'][..])
        .map(|f| usize::from_str(f).unwrap())
        .collect();
    assert_eq!(fields.len(), 4);
    (((MONTH_START_DAY[fields[0] - 1] + (fields[1] - 1)) * 24) + fields[2]) * 60 + fields[3]
}

#[test]
fn test_parse_time() {
    assert_eq!(parse_time("[1518-01-01 00:00]"), 0);
    assert_eq!(parse_time("[1518-01-01 00:10]"), 10);
    assert_eq!(parse_time("[1518-01-01 10:10]"), 610);
    assert_eq!(parse_time("[1518-01-11 10:10]"), 14400 + 610);
    assert_eq!(parse_time("[1518-11-11 10:10]"), 437760 + 14400 + 610);
}

#[derive(Debug, Eq, PartialEq)]
struct Event {
    time: usize,
    action: Action,
}

#[derive(Debug, Eq, PartialEq)]
enum Action {
    Begins(usize),
    FallsAsleep,
    WakesUp,
}

struct Record {
    mins_asleep: usize,
    times_asleep: [usize; 24 * 60],
}

impl Default for Record {
    fn default() -> Record {
        Record {
            mins_asleep: 0,
            times_asleep: [0; 24 * 60],
        }
    }
}

impl Record {
    fn mark(&mut self, slept: Range<usize>) {
        self.mins_asleep += slept.end - slept.start;
        for min in slept {
            self.times_asleep[min % (24 * 60)] += 1;
        }
    }
}

#[derive(Debug)]
enum State {
    Nobody,
    Awake(usize),
    Asleep(usize, usize), // guard, start
}

fn main() -> Result<(), std::io::Error> {
    let stdin = std::io::stdin();
    let mut events = Vec::new();
    for line in stdin.lock().lines() {
        let line = line?;
        assert!(line.len() >= 27);
        let time = parse_time(&line);
        let action = if &line[18..26] == " Guard #" && &line[line.len() - 13..] == " begins shift" {
            Action::Begins(usize::from_str(&line[26..line.len() - 13]).unwrap())
        } else if &line[18..] == " falls asleep" {
            Action::FallsAsleep
        } else if &line[18..] == " wakes up" {
            Action::WakesUp
        } else {
            panic!("Bad line: {}", line)
        };
        events.push(Event { time, action });
    }

    events.sort_by_key(|e| e.time);

    let mut state = State::Nobody;
    let mut records: HashMap<usize, Record> = HashMap::new();

    for event in events {
        match event.action {
            Action::Begins(g) => {
                state = match state {
                    State::Nobody => State::Awake(g),
                    State::Awake(_) => State::Awake(g),
                    State::Asleep(prior, start) => {
                        records
                            .entry(prior)
                            .or_insert_with(Record::default)
                            .mark(start..event.time);
                        State::Awake(g)
                    }
                };
            }
            Action::FallsAsleep => {
                state = match state {
                    State::Nobody => panic!("Who fell asleep??"),
                    State::Asleep(g, _) => panic!("I thought {} was already asleep?", g),
                    State::Awake(guard) => State::Asleep(guard, event.time),
                };
            }
            Action::WakesUp => {
                state = match state {
                    State::Nobody => panic!("Who woke up??"),
                    State::Awake(g) => panic!("I thought {} was already awake?", g),
                    State::Asleep(guard, start) => {
                        records
                            .entry(guard)
                            .or_insert_with(Record::default)
                            .mark(start..event.time);
                        State::Awake(guard)
                    }
                };
            }
        }
    }

    let mut records: Vec<(usize, Record)> = records.into_iter().collect();
    records.sort_by_key(|r| r.1.mins_asleep);

    let sleepiest = &records[records.len() - 1];
    let minute = sleepiest.1.times_asleep[0..60]
        .iter()
        .enumerate()
        .max_by_key(|(_i, c)| *c)
        .unwrap();

    println!(
        "Guard #{} was asleep most often ({} minutes total)",
        sleepiest.0, sleepiest.1.mins_asleep
    );
    println!(
        "They were asleep most often ({} times) at {}:{}",
        minute.1,
        minute.0 / 60,
        minute.0 % 60
    );

    {
        let (guard, record) = records
            .iter()
            .max_by_key(|(_g, r)| r.times_asleep.iter().max())
            .unwrap();
        let (minute, times) = record
            .times_asleep
            .iter()
            .enumerate()
            .max_by_key(|(_i, t)| *t)
            .unwrap();
        println!(
            "Guard #{} was asleep {} times on minute {}",
            guard, times, minute
        );
    }

    Ok(())
}

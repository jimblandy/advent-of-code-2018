extern crate failure;

use failure::Error;
use std::collections::HashSet;
use std::io::Read;
use std::str::FromStr;

fn main() -> Result<(), Error> {
    let mut input = String::new();
    {
        let stdin = std::io::stdin();
        stdin.lock().read_to_string(&mut input)?;
    }

    let changes: Vec<_> = input
        .split_terminator(&[',', '\n'][..])
        .map(|line| isize::from_str(line.trim()).expect("bad number"))
        .collect();

    let frequencies = changes.iter().cycle().scan(0, |s, d| {
        *s += d;
        Some(*s)
    });

    let mut seen = HashSet::new();
    for frequency in frequencies.take(100_000_000) {
        if !seen.insert(frequency) {
            println!("{}", frequency);
            return Ok(());
        }
    }

    println!("never found a repeating value");
    Ok(())
}

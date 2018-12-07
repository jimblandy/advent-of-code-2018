extern crate failure;

use failure::Error;
use std::io::Read;
use std::str::FromStr;

fn main() -> Result<(), Error> {
    let mut input = String::new();
    {
        let stdin = std::io::stdin();
        stdin.lock().read_to_string(&mut input)?;
    }

    let mut frequency = 0;

    for number in input.lines() {
        frequency += isize::from_str(number.trim())?;
    }

    println!("{}", frequency);

    Ok(())
}

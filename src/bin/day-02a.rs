use std::collections::HashMap;
use std::hash::Hash;
use std::io::BufRead;

fn hist<T, I>(iter: I) -> HashMap<T, usize>
where
    I: IntoIterator<Item = T>,
    T: Hash + Eq,
{
    let mut hist = HashMap::new();
    for elt in iter {
        *hist.entry(elt).or_insert(0) += 1;
    }
    hist
}

fn main() -> Result<(), std::io::Error> {
    let stdin = std::io::stdin();
    let mut twos = 0;
    let mut threes = 0;
    for line in stdin.lock().lines() {
        let line = line?;
        let h = hist(hist(line.chars()).values().cloned());
        if let Some(_) = h.get(&2) {
            twos += 1;
        }
        if let Some(_) = h.get(&3) {
            threes += 1;
        }
    }
    println!(
        "twos: {}  threes: {}  checksum: {}",
        twos,
        threes,
        twos * threes
    );
    Ok(())
}

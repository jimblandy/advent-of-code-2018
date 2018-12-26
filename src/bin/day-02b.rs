use std::io::BufRead;

fn diffs(a: &str, b: &str) -> usize {
    assert_eq!(a.len(), b.len());
    a.chars()
        .zip(b.chars())
        .map(|(a, b)| if a == b { 0 } else { 1 })
        .sum()
}

fn main() -> Result<(), std::io::Error> {
    let stdin = std::io::stdin();
    let ids: Vec<_> = stdin.lock().lines().collect::<Result<_, _>>()?;
    for i in 0..ids.len() {
        for j in (i + 1)..ids.len() {
            if diffs(&ids[i], &ids[j]) == 1 {
                println!("{}\n{}", ids[i], ids[j]);
            }
        }
    }
    Ok(())
}

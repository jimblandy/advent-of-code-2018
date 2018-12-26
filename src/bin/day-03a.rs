use std::error::Error;
use std::io::BufRead;
use std::str::FromStr;

#[derive(Debug)]
struct Claim {
    id: usize,
    top: usize,
    left: usize,
    width: usize,
    height: usize,
}

impl Claim {
    fn right(&self) -> usize {
        self.left + self.width
    }
    fn bottom(&self) -> usize {
        self.top + self.height
    }
}

impl FromStr for Claim {
    type Err = Box<Error>;
    fn from_str(s: &str) -> Result<Claim, Self::Err> {
        let fields = s
            .split(&['#', '@', ',', ':', 'x'][..])
            .map(str::trim)
            .collect::<Vec<_>>();
        if fields.len() != 6 {
            return Err(Box::new(std::io::Error::new(
                std::io::ErrorKind::Other,
                format!("bad claim: {:?}", s),
            )));
        }
        Ok(Claim {
            id: usize::from_str(fields[1])?,
            left: usize::from_str(fields[2])?,
            top: usize::from_str(fields[3])?,
            width: usize::from_str(fields[4])?,
            height: usize::from_str(fields[5])?,
        })
    }
}

fn main() -> Result<(), Box<Error>> {
    let stdin = std::io::stdin();
    let lines: Vec<_> = stdin.lock().lines().collect::<Result<_, _>>()?;
    let claims: Vec<_> = lines
        .iter()
        .map(|l| Claim::from_str(&l))
        .collect::<Result<_, _>>()?;

    let width = claims.iter().map(|c| c.right()).max().unwrap();
    let height = claims.iter().map(|c| c.bottom()).max().unwrap();
    println!("overall dimensions: {}x{}", width, height);

    let mut used = Vec::new();
    used.resize(width * height, 0);
    for claim in &claims {
        for r in 0..claim.height {
            for c in 0..claim.width {
                let ix = (claim.top + r) * width + (claim.left + c);
                used[ix] += 1;
            }
        }
    }

    for claim in &claims {
        let mut okay = true;
        for r in 0..claim.height {
            for c in 0..claim.width {
                let ix = (claim.top + r) * width + (claim.left + c);
                assert!(used[ix] >= 1);
                if used[ix] != 1 {
                    okay = false;
                }
            }
        }
        if okay {
            println!("Claim ID #{} seems okay", claim.id);
        }
    }

    println!("{}", used.iter().filter(|c| **c > 1).count());

    Ok(())
}

static TEST_INPUT: &'static str = "dabAcCaCBAcCcaDA";
static INPUT: &'static str = include_str!("day-05.input");

fn reacts(left: char, right: char) -> bool {
    left.is_ascii_alphabetic()
        && right.is_ascii_alphabetic()
        && (left as i32 - right as i32).abs() == 32
}

fn reduce(input: &str) -> String {
    let input: Vec<_> = input.chars().collect();
    let mut output = Vec::new();

    for ch in input {
        if !output.is_empty() && reacts(output[output.len() - 1], ch) {
            output.pop();
        } else {
            output.push(ch);
        }
    }

    output.iter().collect::<String>()
}

fn main() {
    assert_eq!(&reduce(TEST_INPUT), "dabCBAcaDA");
    assert_eq!(&reduce("aA"), "");
    assert_eq!(&reduce("abBA"), "");
    assert_eq!(&reduce("abAB"), "abAB");
    assert_eq!(&reduce("aabAAB"), "aabAAB");

    println!("{} units remaining after reacting", reduce(INPUT).len());

    let (best_unit, length) = (b'a'..b'z')
        .map(|unit| {
            let unit = unit as char;
            let purified: String = INPUT
                .chars()
                .filter(|c| c.to_ascii_lowercase() != unit)
                .collect();
            (unit, reduce(&purified).len())
        })
        .min_by_key(|(_unit, len)| *len)
        .unwrap();

    println!("Dropping {:?} yields a length of {}", best_unit, length);
}

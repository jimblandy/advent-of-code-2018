fn main() {
    let target = 793061;
    let len = 6;
    let modulus = usize::pow(10, len as u32);

    let mut recipes = vec![3, 7];
    let mut tail = 37;

    let mut a = 0;
    let mut b = 1;
    while tail != target {
        let sum = recipes[a] + recipes[b];
        if sum >= 10 {
            recipes.push(1);
            tail = (tail * 10) + 1;
            tail %= modulus;
            if tail == target {
                break;
            }
            recipes.push(sum - 10);
            tail = (tail * 10) + (sum - 10);
        } else {
            recipes.push(sum);
            tail = (tail * 10) + sum;
        }
        tail = tail % modulus;
        a = (a + recipes[a] + 1) % recipes.len();
        b = (b + recipes[b] + 1) % recipes.len();
    }

    println!("{} {}", a, b);
    println!("{}", recipes.len() - len);
    println!("{:?}", &recipes[recipes.len() - 20..]);
    /*
    for score in &recipes[target..target + 10] {
        print!("{}", score);
    }
    println!();
     */
}

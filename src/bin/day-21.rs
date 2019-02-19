extern crate advent_of_code_2018 as aoc;

#[allow(unused_imports)]
use aoc::machine::{assemble, State, Word};

/*
mod input {
    use aoc::machine::AssemblyInsn;
    include!("day-21-input.rsi");
}
*/

fn main() {
    /*
        let machine_code = assemble(input::PROGRAM);
        let mut ip = 0;
        let mut state = State::default();
        state.0[0] = 4103330;
        loop {
            state.0[input::IP] = ip as Word;
            let insn = &machine_code[ip];
            println!("State: {}    insn: {}", state, insn);

            insn.semantic.1.step(insn, &mut state);
            if state.0[input::IP] < 0 {
                break;
            }
            ip = state.0[input::IP] as usize;
            ip += 1;
            if machine_code.len() <= ip {
                break;
            }
        }

        println!("Final state: {:?}", state);
    */

    println!("Rust cartoon:");

    let mut seen = 0;
    let mut r3: Word = 0;
    loop {
        let mut r1 = r3 | 0x10000;
        r3 = 0x903319;
        loop {
            r3 = (((r3 + (r1 & 0xff)) & 0xffffff) * 0x1016b) & 0xffffff;
            if r1 < 256 {
                break;
            }
            r1 = r1 / 256;
        }

        println!("(28) r3 == {}", r3);
        if r3 == 13975413 {
            seen += 1;
            if seen == 3 {
                break;
            }
        }
    }
}

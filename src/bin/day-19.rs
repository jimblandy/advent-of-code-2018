extern crate advent_of_code_2018 as aoc;

use aoc::machine::{assemble, State, Word};

mod input {
    use aoc::machine::AssemblyInsn;
    include!("day-19-input.rsi");
}

fn main() {
    let machine_code = assemble(input::PROGRAM);

    let mut ip = 0;
    let mut state = State::default();
    state.0[0] = 1;
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
}

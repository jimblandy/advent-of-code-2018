use std::fmt;

type Word = isize;

#[derive(Clone, Debug, Default, Eq, PartialEq)]
struct State([Word; 6]);

impl fmt::Display for State {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "({:2})   {:4} {:4} - {:4} {:4} {:4}",
               self.0[2],
               self.0[0],
               self.0[1],
               self.0[3],
               self.0[4],
               self.0[5])
    }
}

trait Semantic {
    fn step(&self, insn: &Insn, state: &mut State);
}

#[derive(Clone)]
pub struct AssemblyInsn {
    mnemonic: &'static str,
    a: u8,
    b: u8,
    c: u8
}

struct Insn<'a> {
    semantic: &'a Entry,
    a: u8,
    b: u8,
    c: u8
}

impl<'a> fmt::Display for Insn<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "{} {} {} {}", self.semantic.0, self.a, self.b, self.c)
    }
}

mod input {
    use super::AssemblyInsn;
    include!("day-19-input.rsi");
}

impl State {
    fn get(&self, reg: u8) -> Word {
        assert!(reg < 6);
        self.0[reg as usize]
    }

    fn set(&mut self, reg: u8, value: Word) {
        assert!(reg < 6);
        self.0[reg as usize] = value;
    }
}

mod ops {
    use super::Word;

    pub fn add(a: Word, b: Word) -> Word {
        a.checked_add(b).unwrap()
    }

    pub fn mul(a: Word, b: Word) -> Word {
        a.checked_mul(b).unwrap()
    }

    pub fn ban(a: Word, b: Word) -> Word {
        a & b
    }

    pub fn bor(a: Word, b: Word) -> Word {
        a | b
    }

    pub fn gt(a: Word, b: Word) -> Word {
        if a > b { 1 } else { 0 }
    }

    pub fn eq(a: Word, b: Word) -> Word {
        if a == b { 1 } else { 0 }
    }

    pub fn set(a: Word) -> Word {
        a
    }
}

mod formats {
    use super::{Insn, Semantic, State, Word};

    pub struct RR<S>(pub S);

    impl<S> Semantic for RR<S>
    where S: Fn(Word, Word) -> Word
    {
        fn step(&self, insn: &Insn, state: &mut State) {
            let value = self.0(state.get(insn.a), state.get(insn.b));
            state.set(insn.c, value);
        }
    }

    pub struct RI<S>(pub S);

    impl<S> Semantic for RI<S>
    where S: Fn(Word, Word) -> Word
    {
        fn step(&self, insn: &Insn, state: &mut State) {
            let value = self.0(state.get(insn.a), insn.b as Word);
            state.set(insn.c, value);
        }
    }

    pub struct IR<S>(pub S);

    impl<S> Semantic for IR<S>
    where S: Fn(Word, Word) -> Word
    {
        fn step(&self, insn: &Insn, state: &mut State) {
            let value = self.0(insn.a as Word, state.get(insn.b));
            state.set(insn.c, value);
        }
    }

    pub struct I<S>(pub S);

    impl<S> Semantic for I<S>
    where S: Fn(Word) -> Word
    {
        fn step(&self, insn: &Insn, state: &mut State) {
            let value = self.0(insn.a as Word);
            state.set(insn.c, value);
        }
    }

    pub struct R<S>(pub S);

    impl<S> Semantic for R<S>
    where S: Fn(Word) -> Word
    {
        fn step(&self, insn: &Insn, state: &mut State) {
            let value = self.0(state.get(insn.a));
            state.set(insn.c, value);
        }
    }
}

macro_rules! insn {
    ($name:ident : $op:ident $format:ident) => {
        ( stringify!($name), Box::new(formats::$format(ops::$op)) )
    }
}

type Entry = (&'static str, Box<dyn Semantic>);
fn insn_by_mnemonic() -> Vec<Entry> {
    vec![
        insn!(addi: add RI),
        insn!(muli: mul RI),
        insn!(bani: ban RI),
        insn!(bori: bor RI),

        insn!(addr: add RR),
        insn!(mulr: mul RR),
        insn!(banr: ban RR),
        insn!(borr: bor RR),

        insn!(seti: set I),
        insn!(setr: set R),

        insn!(gtir: gt IR),
        insn!(gtri: gt RI),
        insn!(gtrr: gt RR),
        insn!(eqir: eq IR),
        insn!(eqri: eq RI),
        insn!(eqrr: eq RR),
    ]
}

fn assemble<'a>(asm: &'a [AssemblyInsn],
                table: &'a [(&'static str, Box<dyn Semantic>)])
                -> Vec<Insn<'a>>
{
    asm
        .iter()
        .cloned()
        .map(|AssemblyInsn { mnemonic, a, b, c }| {
            let entry = table
                .iter()
                .find(|(name, _sem)| name == &mnemonic)
                .expect("Unrecognized mnemonic?");
            Insn { semantic: &entry, a, b, c }
        })
        .collect::<Vec<_>>()
}

fn main() {
    let insns = insn_by_mnemonic();
    let machine_code = assemble(input::PROGRAM, &insns);

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

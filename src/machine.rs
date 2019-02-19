use std::fmt;

pub type Word = isize;

#[derive(Clone, Debug, Default, Eq, PartialEq)]
pub struct State(pub [Word; 6]);

impl fmt::Display for State {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(
            f,
            "({:2})   {:8} {:8} {:8} - {:8} {:8}",
            self.0[5], self.0[0], self.0[1], self.0[2], self.0[3], self.0[4]
        )
    }
}

pub trait Semantic: Sync + Send {
    fn step(&self, insn: &Insn, state: &mut State);
}

#[derive(Clone)]
pub struct AssemblyInsn {
    pub mnemonic: &'static str,
    pub a: Word,
    pub b: Word,
    pub c: Word,
}

pub struct Insn<'a> {
    pub semantic: &'a Entry,
    pub a: Word,
    pub b: Word,
    pub c: Word,
}

impl<'a> fmt::Display for Insn<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "{} {} {} {}", self.semantic.0, self.a, self.b, self.c)
    }
}

impl State {
    fn get(&self, reg: Word) -> Word {
        assert!(reg < 6);
        self.0[reg as usize]
    }

    fn set(&mut self, reg: Word, value: Word) {
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
        if a > b {
            1
        } else {
            0
        }
    }

    pub fn eq(a: Word, b: Word) -> Word {
        if a == b {
            1
        } else {
            0
        }
    }

    pub fn set(a: Word) -> Word {
        a
    }
}

mod formats {
    use super::{Insn, Semantic, State, Word};

    pub struct RR<S>(pub S);

    impl<S: Sync + Send> Semantic for RR<S>
    where
        S: Fn(Word, Word) -> Word,
    {
        fn step(&self, insn: &Insn, state: &mut State) {
            let value = self.0(state.get(insn.a), state.get(insn.b));
            state.set(insn.c, value);
        }
    }

    pub struct RI<S>(pub S);

    impl<S: Sync + Send> Semantic for RI<S>
    where
        S: Fn(Word, Word) -> Word,
    {
        fn step(&self, insn: &Insn, state: &mut State) {
            let value = self.0(state.get(insn.a), insn.b as Word);
            state.set(insn.c, value);
        }
    }

    pub struct IR<S>(pub S);

    impl<S: Sync + Send> Semantic for IR<S>
    where
        S: Fn(Word, Word) -> Word,
    {
        fn step(&self, insn: &Insn, state: &mut State) {
            let value = self.0(insn.a as Word, state.get(insn.b));
            state.set(insn.c, value);
        }
    }

    pub struct I<S>(pub S);

    impl<S: Sync + Send> Semantic for I<S>
    where
        S: Fn(Word) -> Word,
    {
        fn step(&self, insn: &Insn, state: &mut State) {
            let value = self.0(insn.a as Word);
            state.set(insn.c, value);
        }
    }

    pub struct R<S>(pub S);

    impl<S: Sync + Send> Semantic for R<S>
    where
        S: Fn(Word) -> Word,
    {
        fn step(&self, insn: &Insn, state: &mut State) {
            let value = self.0(state.get(insn.a));
            state.set(insn.c, value);
        }
    }
}

macro_rules! insn {
    ($name:ident : $op:ident $format:ident) => {
        (stringify!($name), &(formats::$format(ops::$op)))
    };
}

type Entry = (&'static str, &'static dyn Semantic);
static INSN_BY_MNEMONIC: &[Entry] = &[
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
];

pub fn assemble<'a>(asm: &'a [AssemblyInsn]) -> Vec<Insn<'a>> {
    asm.iter()
        .cloned()
        .map(|AssemblyInsn { mnemonic, a, b, c }| {
            let entry = INSN_BY_MNEMONIC
                .iter()
                .find(|(name, _sem)| name == &mnemonic)
                .expect("Unrecognized mnemonic?");
            Insn {
                semantic: &entry,
                a,
                b,
                c,
            }
        })
        .collect::<Vec<_>>()
}

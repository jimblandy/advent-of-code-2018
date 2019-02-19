type Word = isize;

#[derive(Clone, Debug, Default, Eq, PartialEq)]
struct State([Word; 4]);

struct Insn {
    opcode: u8,
    a: u8,
    b: u8,
    c: u8,
}

struct SampleExecution {
    before: State,
    insn: Insn,
    after: State,
}

trait Semantic {
    fn step(&self, insn: &Insn, state: &mut State);
}

include!("day-16-input.rsi");

impl State {
    fn get(&self, reg: u8) -> Word {
        assert!(reg < 4);
        self.0[reg as usize]
    }

    fn set(&mut self, reg: u8, value: Word) {
        assert!(reg < 4);
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

    pub fn set(a: Word, _b: Word) -> Word {
        a
    }
}

mod formats {
    use super::{Insn, Semantic, State, Word};

    pub struct RR<S>(pub S);

    impl<S> Semantic for RR<S>
    where
        S: Fn(Word, Word) -> Word,
    {
        fn step(&self, insn: &Insn, state: &mut State) {
            let value = self.0(state.get(insn.a), state.get(insn.b));
            state.set(insn.c, value);
        }
    }

    pub struct RI<S>(pub S);

    impl<S> Semantic for RI<S>
    where
        S: Fn(Word, Word) -> Word,
    {
        fn step(&self, insn: &Insn, state: &mut State) {
            let value = self.0(state.get(insn.a), insn.b as Word);
            state.set(insn.c, value);
        }
    }

    pub struct IR<S>(pub S);

    impl<S> Semantic for IR<S>
    where
        S: Fn(Word, Word) -> Word,
    {
        fn step(&self, insn: &Insn, state: &mut State) {
            let value = self.0(insn.a as Word, state.get(insn.b));
            state.set(insn.c, value);
        }
    }
}

macro_rules! insn {
    ($name:ident : $op:ident $format:ident) => {
        (stringify!($name), Box::new(formats::$format(ops::$op)))
    };
}

fn all_semantics() -> Vec<(&'static str, Box<dyn Semantic>)> {
    vec![
        insn!(addi: add RI),
        insn!(muli: mul RI),
        insn!(bani: ban RI),
        insn!(bori: bor RI),
        insn!(addr: add RR),
        insn!(mulr: mul RR),
        insn!(banr: ban RR),
        insn!(borr: bor RR),
        insn!(seti: set IR),
        insn!(setr: set RR),
        insn!(gtir: gt IR),
        insn!(gtri: gt RI),
        insn!(gtrr: gt RR),
        insn!(eqir: eq IR),
        insn!(eqri: eq RI),
        insn!(eqrr: eq RR),
    ]
}

impl SampleExecution {
    fn behaves_like(&self, sem: &Box<dyn Semantic>) -> bool {
        let mut state = self.before.clone();
        sem.step(&self.insn, &mut state);
        state == self.after
    }
}

fn main() {
    let semantics = all_semantics();

    let example = SampleExecution {
        before: State([3, 2, 1, 1]),
        insn: Insn {
            opcode: 9,
            a: 2,
            b: 1,
            c: 2,
        },
        after: State([3, 2, 2, 1]),
    };

    println!("Example step behaves like:");
    for (name, sem) in &semantics {
        if example.behaves_like(sem) {
            println!("    {}", name);
        }
    }

    let mut count = 0;
    for (i, execution) in SAMPLE_EXECUTIONS.iter().enumerate() {
        if semantics
            .iter()
            .filter(|(_name, sem)| execution.behaves_like(sem))
            .count()
            >= 3
        {
            count += 1;
            println!(
                "Sample execution #{} behaves like three or more instructions:",
                i
            );
            for name in semantics
                .iter()
                .filter(|(_name, sem)| execution.behaves_like(sem))
                .map(|(name, _sem)| name)
            {
                println!("    {}", name);
            }
        }
    }

    println!(
        "In total, {} samples behave like three or or more opcodes.",
        count
    );

    let mut possible = all_semantics()
        .into_iter()
        .map(|(name, sem)| (name, sem, [true; 16]))
        .collect::<Vec<_>>();
    for sample in SAMPLE_EXECUTIONS {
        for (_name, sem, opcodes) in &mut possible {
            if !sample.behaves_like(sem) {
                opcodes[sample.insn.opcode as usize] = false;
            }
        }
    }

    // Count the number of opcodes possible for each semantic.
    let mut possible = possible
        .into_iter()
        .map(|(name, sem, opcodes)| (name, sem, opcodes, opcodes.iter().filter(|&&f| f).count()))
        .collect::<Vec<_>>();

    // When a semantic has only one possible opcode, that means that opcode
    // isn't a possibility for any other. This can disambiguate other semantics'
    // opcodes, so we must iterate to closure.
    let mut worklist = possible
        .iter()
        .enumerate()
        .filter(|(_i, (_name, _sem, _opcodes, count))| *count == 1)
        .map(|(i, _)| i)
        .collect::<Vec<_>>();
    while !worklist.is_empty() {
        let sem_ix = worklist.pop().unwrap();
        let op = {
            let (_name, _sem, opcodes, count) = &mut possible[sem_ix];
            assert_eq!(*count, 1);
            let mut ops = opcodes
                .iter()
                .enumerate()
                .filter(|(_i, f)| **f)
                .map(|(i, _f)| i);
            let op = ops.next().expect("wait, count was at least 1");
            assert!(ops.next().is_none());
            op
        };

        for (i, (_name, _sem, opcodes, count)) in possible.iter_mut().enumerate() {
            if i != sem_ix {
                if opcodes[op] {
                    *count -= 1;
                    if *count == 1 {
                        worklist.push(i);
                    }
                }
                opcodes[op] = false;
            }
        }
    }

    // Reduce each bitmap to a single value.
    let mut possible = possible
        .into_iter()
        .map(|(name, sem, opcodes, count)| {
            assert_eq!(count, 1);
            let mut ops = opcodes
                .iter()
                .enumerate()
                .filter(|(_i, f)| **f)
                .map(|(i, _f)| i);
            let op = ops.next().expect("wait, count was 1");
            assert!(ops.next().is_none());
            (name, sem, op)
        })
        .collect::<Vec<_>>();

    possible.sort_by_key(|(_name, _sem, op)| *op);

    // Build a table mapping opcodes to names and semantics.
    assert_eq!(possible.len(), 16); // map is complete
    let map = possible
        .into_iter()
        .scan(0, |state, (name, sem, op)| {
            assert_eq!(op, *state);
            *state += 1;
            Some((name, sem))
        })
        .collect::<Vec<_>>();

    // Print out the opcode map.
    println!("Opcode map:");
    for (op, (name, _sem)) in map.iter().enumerate() {
        println!("  {}: {}", op, name);
    }

    // Run the sample program.
    let mut state = State::default();
    for insn in SAMPLE_PROGRAM {
        map[insn.opcode as usize].1.step(insn, &mut state);
    }

    println!("Final state: {:?}", state);
}

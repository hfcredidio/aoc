use std::collections::HashSet;
use std::fs::File;
use std::io::BufRead;
use std::io::BufReader;
use std::path::Path;

#[derive(Debug, Clone, Copy)]
enum OpCode {
    Nop(isize),
    Acc(i32),
    Jmp(isize),
}

impl OpCode {
    pub fn flip(self) -> Self {
        match self {
            OpCode::Nop(val) => OpCode::Jmp(val),
            OpCode::Acc(val) => OpCode::Acc(val),
            OpCode::Jmp(val) => OpCode::Nop(val),
        }
    }
}

#[derive(Debug, Clone, Copy)]
struct State {
    a: i32,
    pc: usize,
}

fn parse_line(line: String) -> OpCode {
    match &line[0..3] {
        "nop" => OpCode::Nop((&line[4..]).parse::<isize>().unwrap()),
        "acc" => OpCode::Acc((&line[4..]).parse::<i32>().unwrap()),
        "jmp" => OpCode::Jmp((&line[4..]).parse::<isize>().unwrap()),
        _ => panic!("Op code not recognized"),
    }
}

fn update_state(s: State, op: OpCode) -> State {
    match op {
        OpCode::Nop(_) => State {
            a: s.a,
            pc: s.pc + 1,
        },
        OpCode::Acc(value) => State {
            a: s.a + value,
            pc: s.pc + 1,
        },
        OpCode::Jmp(value) => State {
            a: s.a,
            pc: (s.pc as isize + value) as usize,
        },
    }
}

fn terminates(program: &Vec<OpCode>) -> (bool, State) {
    let mut visited_pc = HashSet::new();
    let mut state = State { a: 0, pc: 0 };
    while !visited_pc.contains(&state.pc) && state.pc < program.len() {
        visited_pc.insert(state.pc);
        state = update_state(state, program[state.pc]);
    }
    (state.pc == program.len(), state)
}

fn fix(mut program: Vec<OpCode>) -> Option<State> {
    for i in 0..program.len() {
        match program[i] {
            OpCode::Acc(_) => continue,
            _ => {
                program[i] = program[i].flip();
                let (term, st) = terminates(&program);
                if term {
                    return Some(st);
                } else {
                    program[i] = program[i].flip();
                };
            }
        }
    }
    None
}

pub fn day8(path: &Path) {
    let file = File::open(path).unwrap();
    let program: Vec<OpCode> = BufReader::new(file)
        .lines()
        .map(|x| x.unwrap())
        .map(parse_line)
        .collect();

    println!("{:?}", terminates(&program));
    println!("{:?}", fix(program));
}

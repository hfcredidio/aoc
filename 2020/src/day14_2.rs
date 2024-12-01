use std::collections::HashMap;
use std::fs::File;
use std::io::BufRead;
use std::io::BufReader;
use std::path::Path;
use std::str;

#[derive(Debug)]
enum Operation {
    SetBitMask(BitMask),
    SetMemory(u64, u64),
}

impl Operation {
    pub fn from_str(s: &str) -> Self {
        match &s[0..3] {
            "mas" => Operation::SetBitMask(BitMask::from_str(&s[7..])),
            "mem" => {
                let mut it = s.chars();
                let addr = it
                    .by_ref()
                    .skip_while(|c| *c != '[')
                    .skip(1)
                    .take_while(|c| *c != ']')
                    .collect::<String>()
                    .parse::<u64>()
                    .unwrap();
                let value = it
                    .skip_while(|c| *c != '=')
                    .skip(2)
                    .collect::<String>()
                    .parse::<u64>()
                    .unwrap();
                Operation::SetMemory(addr, value)
            }
            _ => panic!(""),
        }
    }
}

#[derive(Debug)]
struct BitMask {
    mask: u64,
    floating_bits: Vec<usize>,
}

fn list_possibilities(floating_bits: &[usize], value: u64) -> Vec<u64> {
    if floating_bits.len() == 0 {
        vec![value]
    } else {
        let mut values = vec![];
        let bit = floating_bits[0];
        values.extend_from_slice(&list_possibilities(&floating_bits[1..], value | (1 << bit)));
        values.extend_from_slice(&list_possibilities(
            &floating_bits[1..],
            value & !(1 << bit),
        ));
        values
    }
}

impl BitMask {
    pub fn from_str(s: &str) -> Self {
        let mask: u64 = s
            .chars()
            .map(|c| match c {
                'X' => 0,
                '0' => 0,
                '1' => 1,
                _ => panic!(""),
            })
            .fold(0, |state, dig| (state << 1) | dig);
        let floating_bits = s
            .chars()
            .enumerate()
            .filter(|(_, c)| *c == 'X')
            .map(|(i, _)| 35 - i)
            .collect::<Vec<usize>>();
        BitMask {
            mask: mask,
            floating_bits: floating_bits,
        }
    }
    pub fn apply_and_list_possibilities(&self, x: u64) -> Vec<u64> {
        return list_possibilities(&self.floating_bits, x | self.mask);
    }
}

#[derive(Debug)]
struct System {
    bitmask: BitMask,
    memory: HashMap<u64, u64>,
}

impl System {
    pub fn apply(mut self, op: Operation) -> Self {
        match op {
            Operation::SetBitMask(bm) => {
                self.bitmask = bm;
            }
            Operation::SetMemory(addr, value) => {
                for a in self.bitmask.apply_and_list_possibilities(addr) {
                    self.memory.insert(a, value);
                }
            }
        }
        self
    }
}

pub fn day14_2(path: &Path) {
    let file = File::open(path).unwrap();
    let state = BufReader::new(file)
        .lines()
        .map(|l| Operation::from_str(l.unwrap().as_str()))
        .fold(
            System {
                bitmask: BitMask {
                    mask: 0,
                    floating_bits: vec![],
                },
                memory: HashMap::new(),
            },
            |state, action| state.apply(action),
        );
    println!("{:?}", state.memory.iter().map(|(_, v)| v).sum::<u64>());
}

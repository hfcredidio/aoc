use std::collections::HashMap;
use std::fs::File;
use std::io::BufRead;
use std::io::BufReader;
use std::path::Path;
use std::str;

#[derive(Debug)]
struct BitMask {
    zero_mask: u64,
    one_mask: u64,
}

impl BitMask {
    pub fn from_str(s: &str) -> Self {
        let omask: u64 = s
            .chars()
            .map(|c| match c {
                'X' => 0,
                '0' => 0,
                '1' => 1,
                _ => panic!(""),
            })
            .fold(0, |state, dig| (state << 1) | dig);
        let zmask: u64 = s
            .chars()
            .map(|c| match c {
                'X' => 1,
                '0' => 0,
                '1' => 1,
                _ => panic!(""),
            })
            .fold(0, |state, dig| (state << 1) | dig);
        BitMask {
            zero_mask: zmask,
            one_mask: omask,
        }
    }
    pub fn apply(&self, x: u64) -> u64 {
        (x & self.zero_mask) | self.one_mask
    }
}

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
                let masked_value = self.bitmask.apply(value);
                self.memory.insert(addr, masked_value);
            }
        }
        self
    }
}

pub fn day14(path: &Path) {
    let file = File::open(path).unwrap();
    let state = BufReader::new(file)
        .lines()
        .map(|l| Operation::from_str(l.unwrap().as_str()))
        .fold(
            System {
                bitmask: BitMask {
                    zero_mask: 0,
                    one_mask: 0,
                },
                memory: HashMap::new(),
            },
            |state, action| state.apply(action),
        );
    println!("{:?}", state.memory.iter().map(|(_, v)| v).sum::<u64>());
}

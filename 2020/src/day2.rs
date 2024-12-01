use std::fs::File;
use std::io::BufRead;
use std::io::BufReader;
use std::iter::Peekable;
use std::path::Path;
use std::str::Chars;

trait PasswordValidation {
    fn from_parsed_string(num1: u32, num2: u32, letter: char) -> Self;
    fn validate(&self, pw: &String) -> bool;
}

struct PasswordPolicy {
    letter: char,
    min_count: u32,
    max_count: u32,
}

impl PasswordValidation for PasswordPolicy {
    fn from_parsed_string(num1: u32, num2: u32, letter: char) -> Self {
        Self {
            letter,
            min_count: num1,
            max_count: num2,
        }
    }

    fn validate(&self, pw: &String) -> bool {
        let count = pw.chars().filter(|c| *c == self.letter).count() as u32;
        self.min_count <= count && count <= self.max_count
    }
}

struct TobogganPasswordPolicy {
    letter: u8,
    index1: usize,
    index2: usize,
}

impl PasswordValidation for TobogganPasswordPolicy {
    fn from_parsed_string(num1: u32, num2: u32, letter: char) -> Self {
        Self {
            letter: letter as u8,
            index1: num1 as usize - 1,
            index2: num2 as usize - 1,
        }
    }

    fn validate(&self, pw: &String) -> bool {
        let bpw = pw.as_bytes();
        (bpw[self.index1] == self.letter) != (bpw[self.index2] == self.letter)
    }
}

fn parse_dig(mut it: Peekable<Chars>) -> (Result<u32, String>, Peekable<Chars>) {
    match it.peek() {
        Some(c) => match c.to_digit(10) {
            Some(d) => {
                it.next();
                (Ok(d), it)
            }
            None => (Err("Not a digit".to_string()), it),
        },
        None => (Err("End of input".to_string()), it),
    }
}

fn parse_int(it: Peekable<Chars>) -> (Result<u32, String>, Peekable<Chars>) {
    let (mut res, mut it) = match parse_dig(it) {
        (Ok(d), nit) => (d, nit),
        (err, nit) => return (err, nit),
    };
    loop {
        let (_res, _it) = match parse_dig(it) {
            (Ok(d), nit) => (res * 10 + d, nit),
            (_, nit) => return (Ok(res), nit),
        };
        res = _res;
        it = _it;
    }
}

fn parse_line<V: PasswordValidation>(line: String) -> (V, String) {
    let it = line.chars().peekable();
    let (num1, mut it) = match parse_int(it) {
        (Err(err), _) => panic!(err),
        (Ok(value), iit) => (value, iit),
    };

    assert_eq!(it.next().unwrap(), '-');

    let (num2, mut it) = match parse_int(it) {
        (Err(err), _) => panic!(err),
        (Ok(value), iit) => (value, iit),
    };

    assert_eq!(it.next().unwrap(), ' ');

    let letter = it.next().unwrap();

    assert_eq!(it.next().unwrap(), ':');
    assert_eq!(it.next().unwrap(), ' ');

    let rest = it.fold("".to_string(), |mut acc, c| {
        acc.push(c);
        acc
    });
    (V::from_parsed_string(num1, num2, letter), rest)
}

fn count_valid<V: PasswordValidation>(path: &Path) -> usize {
    let file = File::open(path).unwrap();
    BufReader::new(file)
        .lines()
        .map(|x| x.unwrap())
        .map(parse_line::<V>)
        .filter(|(pol, pw)| pol.validate(&pw))
        .count()
}

pub fn day2(path: &Path) {
    println!("{}", count_valid::<PasswordPolicy>(&path));
    println!("{}", count_valid::<TobogganPasswordPolicy>(&path));
}

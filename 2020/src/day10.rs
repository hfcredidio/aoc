use std::collections::HashMap;
use std::fs::File;
use std::io::BufRead;
use std::io::BufReader;
use std::iter::Peekable;
use std::path::Path;

pub fn day10(path: &Path) {
    let sorted_numbers = || {
        let file = File::open(path).unwrap();
        let mut numbers: Vec<i64> = BufReader::new(file)
            .lines()
            .map(|x| x.unwrap().parse::<i64>().unwrap())
            .collect();
        numbers.sort();
        numbers
    };

    let numbers: Vec<i64> = sorted_numbers();

    let mut count = HashMap::new();
    *count.entry(numbers[0]).or_insert(0) += 1;
    *count.entry(3).or_insert(0) += 1;
    numbers.windows(2).for_each(|w| {
        let diff = w[1] - w[0];
        *count.entry(diff).or_insert(0) += 1;
    });
    let value = *count.entry(3).or_insert(0) * *count.entry(1).or_insert(0);
    println!("{:?}", value);

    println!("{:?}", combinations(0, numbers.iter().copied().peekable()));
    println!("{:?}", combinations2(&numbers));
}

fn peek_take_while<I, F>(peek: &mut Peekable<I>, accept: F) -> Vec<I::Item>
where
    I: Iterator,
    F: Fn(&I::Item) -> bool + Copy,
{
    let mut out = Vec::new();
    loop {
        match peek.next_if(accept) {
            None => return out,
            Some(value) => out.push(value),
        }
    }
}

fn combinations<I>(outlet: i64, mut adaptors: Peekable<I>) -> i64
where
    I: Iterator<Item = i64>,
{
    let possible: Vec<i64> = peek_take_while(&mut adaptors, |x| *x - outlet <= 3);
    match possible.len() {
        3 => match adaptors.peek() {
            None => 6,
            Some(value) if value - possible[2] == 1 => 7 * combinations(possible[2], adaptors),
            Some(value) if value - possible[2] == 3 => 4 * combinations(possible[2], adaptors),
            _ => panic!("Invalid sequence of adaptors."),
        },
        2 => 2 * combinations(possible[1], adaptors),
        1 => combinations(possible[0], adaptors),
        0 => 1,
        _ => panic!("Invalid sequence of adaptors."),
    }
}

fn combinations2(adaptors: &Vec<i64>) -> i64 {
    let mut map: HashMap<i64, i64> = HashMap::new();
    map.insert(0, 1);
    for v in adaptors.iter().copied() {
        map.insert(
            v,
            map.get(&(v - 1)).unwrap_or(&0)
                + map.get(&(v - 2)).unwrap_or(&0)
                + map.get(&(v - 3)).unwrap_or(&0),
        );
    }
    *map.get(adaptors.last().unwrap()).unwrap()
}

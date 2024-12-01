use std::fs::File;
use std::io::BufRead;
use std::io::BufReader;
use std::path::Path;
use std::str::Chars;

fn seat_id(row: u32, col: u32) -> u32 {
    row * 8 + col
}

fn _parse_position(s: &mut Chars, irow: u32, icol: u32) -> (u32, u32) {
    match s.next() {
        None => (irow, icol),
        Some('B') => _parse_position(s, (irow << 1) | 1, icol),
        Some('F') => _parse_position(s, irow << 1, icol),
        Some('L') => _parse_position(s, irow, icol << 1),
        Some('R') => _parse_position(s, irow, (icol << 1) | 1),
        _ => panic!("Invalid character!"),
    }
}

fn parse_position(s: &mut Chars) -> (u32, u32) {
    _parse_position(s, 0, 0)
}

fn find_vacant(sorted_ids: &Vec<u32>) -> Option<u32> {
    let mut vacant = sorted_ids.windows(2).filter(|ids| ids[1] - ids[0] == 2);
    match vacant.next() {
        None => None,
        Some(ids) => Some(ids[0] + 1),
    }
}

pub fn day5(path: &Path) {
    let file = File::open(path).unwrap();
    let mut ids: Vec<u32> = BufReader::new(file)
        .lines()
        .map(|x| x.unwrap())
        .map(|line| parse_position(&mut line.chars()))
        .map(|(row, col)| seat_id(row, col))
        .collect();
    ids.sort();
    println!("{:?}", ids.last());
    println!("{:?}", find_vacant(&ids));
}

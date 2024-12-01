use std::collections::HashSet;
use std::fs::File;
use std::io::Read;
use std::path::Path;

fn unique_chars(s: &str) -> HashSet<char> {
    s.chars().filter(|c| *c != '\n').collect()
}

fn unique_intersection(s: &str) -> HashSet<char> {
    s.lines()
        .map(unique_chars)
        .fold_first(|acc, x| acc.intersection(&x).copied().collect())
        .unwrap()
}

pub fn day6(path: &Path) {
    let mut data = Vec::new();
    let mut file = File::open(path).unwrap();
    file.read_to_end(&mut data).unwrap();
    let count: u32 = String::from_utf8(data.clone())
        .unwrap()
        .split("\n\n")
        .map(unique_chars)
        .map(|u| u.len() as u32)
        .sum();
    println!("{:?}", count);

    let res: u32 = String::from_utf8(data)
        .unwrap()
        .split("\n\n")
        .map(unique_intersection)
        .map(|u| u.len() as u32)
        .sum();
    println!("{:?}", res);
}

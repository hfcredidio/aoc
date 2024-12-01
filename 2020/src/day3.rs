use std::fs::File;
use std::io::BufRead;
use std::io::BufReader;
use std::path::Path;

fn count_trees(path: &Path, vstep: usize, hstep: usize) -> usize {
    let file = File::open(path).unwrap();
    let mut cur_col = 0;
    BufReader::new(file)
        .lines()
        .map(|x| x.unwrap())
        .step_by(vstep)
        .filter(|line| {
            let is_tree = line.as_bytes()[cur_col] == '#' as u8;
            cur_col = (cur_col + hstep) % line.len();
            is_tree
        })
        .count()
}

pub fn day3(path: &Path) {
    println!("{}", count_trees(path, 1, 3));
    let result = count_trees(path, 1, 1)
        * count_trees(path, 1, 3)
        * count_trees(path, 1, 5)
        * count_trees(path, 1, 7)
        * count_trees(path, 2, 1);
    println!("{}", result);
}

use std::collections::HashSet;
use std::collections::LinkedList;
use std::fs::File;
use std::io::BufRead;
use std::io::BufReader;
use std::path::Path;

fn hastwosum(arr: &LinkedList<i64>, target: i64) -> bool {
    let mut residuals = HashSet::new();
    for n in arr {
        let residual = target - n;
        if residuals.contains(n) {
            return true;
        }
        residuals.insert(residual);
    }
    false
}

fn find_invalid<I>(numbers: I, preamble: usize) -> i64
where
    I: IntoIterator<Item = i64>,
{
    let mut it = numbers.into_iter();
    let mut list: LinkedList<i64> = it.by_ref().take(preamble).collect::<LinkedList<i64>>();
    it.filter(|x| {
        if hastwosum(&list, *x) {
            list.pop_front();
            list.push_back(*x);
            false
        } else {
            true
        }
    })
    .next()
    .unwrap()
}

fn find_sum<I>(x: I, target: i64) -> LinkedList<i64>
where
    I: IntoIterator<Item = i64>,
{
    let mut list = LinkedList::new();
    let mut sum = 0;
    for x in x.into_iter() {
        list.push_back(x);
        sum += x;
        while sum > target {
            match list.pop_front() {
                Some(value) => sum -= value,
                None => sum = 0,
            }
        }
        if sum == target && list.len() > 1 {
            return list;
        }
    }
    panic!("not found!");
}

pub fn day9(path: &Path) {
    let numbers_it = || {
        let file = File::open(path).unwrap();
        BufReader::new(file)
            .lines()
            .map(|x| x.unwrap().parse::<i64>().unwrap())
    };

    let invalid = find_invalid(numbers_it(), 25);
    println!("{:?}", invalid);

    let fin = find_sum(numbers_it(), invalid);
    println!(
        "{:?}",
        fin.iter().max().unwrap() + fin.iter().min().unwrap()
    );
}

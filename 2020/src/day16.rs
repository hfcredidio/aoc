use std::collections::HashMap;
use std::collections::HashSet;
use std::fs::read_to_string;
use std::ops::Range;
use std::path::Path;
use std::str::Chars;

fn parse_range(s: &mut Chars) -> Range<u64> {
    let start = s
        .by_ref()
        .skip_while(|c| !c.is_digit(10))
        .take_while(|c| c.is_digit(10))
        .collect::<String>()
        .parse::<u64>()
        .unwrap();
    let end = s
        .by_ref()
        .take_while(|c| c.is_digit(10))
        .collect::<String>()
        .parse::<u64>()
        .unwrap()
        + 1;
    start..end
}

fn parse_field(fld: &str) -> (String, (Range<u64>, Range<u64>)) {
    let mut s = fld.chars();
    let name = s.by_ref().take_while(|c| *c != ':').collect::<String>();
    let range1 = parse_range(&mut s);
    let range2 = parse_range(&mut s);
    (name, (range1, range2))
}

fn parse_ticket(s: &str) -> Vec<u64> {
    s.chars()
        .skip_while(|c| !c.is_digit(10))
        .collect::<String>()
        .split(",")
        .map(|v| v.parse::<u64>().unwrap())
        .collect::<Vec<u64>>()
}

fn possible_fields(
    fields: &HashMap<String, (Range<u64>, Range<u64>)>,
    value: u64,
) -> HashSet<String> {
    fields
        .iter()
        .filter(|(_, ranges)| ranges.0.contains(&value) || ranges.1.contains(&value))
        .map(|(name, _)| name)
        .cloned()
        .collect::<HashSet<_>>()
}

pub fn day16(path: &Path) {
    let data = read_to_string(path).unwrap();
    let mut it = data.split("\n\n").into_iter();
    let fields = it
        .next()
        .unwrap()
        .split("\n")
        .map(parse_field)
        .collect::<HashMap<_, _>>();
    let your_ticket = parse_ticket(it.by_ref().next().unwrap());
    let nearby_tickets = it
        .next()
        .unwrap()
        .split("\n")
        .skip(1)
        .filter(|l| l.len() != 0)
        .map(parse_ticket)
        .collect::<Vec<Vec<u64>>>();

    println!("{:?}", fields);
    println!("{:?}", your_ticket);
    println!("{:?}", nearby_tickets);

    let invalid = nearby_tickets
        .iter()
        .map(|ticket| {
            ticket
                .iter()
                .filter(|x| possible_fields(&fields, **x).len() == 0)
                .sum::<u64>()
        })
        .sum::<u64>();
    println!("{:?}", invalid);

    let valid_nearby_tickets = nearby_tickets
        .iter()
        .map(|ticket| {
            ticket
                .iter()
                .map(|x| possible_fields(&fields, *x))
                .collect::<Vec<_>>()
        })
        .filter(|fields| fields.iter().map(|f| f.len()).min().unwrap() != 0)
        .fold_first(|acc, fields| {
            fields
                .iter()
                .zip(acc.iter())
                .map(|(f, a)| a.intersection(&f).cloned().collect())
                .collect::<Vec<_>>()
        })
        .unwrap();
    let mut valid_nearby_tickets = valid_nearby_tickets.iter().enumerate().collect::<Vec<_>>();
    valid_nearby_tickets.sort_by_key(|(_, v)| v.len());
    let result = valid_nearby_tickets
        .iter()
        .scan(HashSet::new(), |state, (i, fields)| {
            let res = fields.difference(&state).cloned().next().unwrap();
            *state = (**fields).clone();
            Some((i, res))
        })
        .filter(|(_, field)| field.starts_with("departure"))
        .map(|(i, _)| your_ticket[*i as usize])
        .fold_first(|state, x| state * x);
    println!("{:?}", result);
}

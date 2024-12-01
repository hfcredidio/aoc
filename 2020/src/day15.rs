use std::collections::HashMap;

fn insert_next(game: &mut HashMap<u32, Vec<u32>>, turn: u32, prev: u32) -> u32 {
    match game[&prev].len() {
        0 => panic!(),
        1 => {
            game.entry(0)
                .and_modify(|v| v.push(turn))
                .or_insert(vec![turn]);
            0
        }
        _ => {
            let x = &game[&prev];
            let val = x[x.len() - 1] - x[x.len() - 2];
            game.entry(val)
                .and_modify(|v| v.push(turn))
                .or_insert(vec![turn]);
            val
        }
    }
}

pub fn day15() {
    let input = [8, 13, 1, 0, 18, 9];
    let mut memory: HashMap<u32, Vec<u32>> = input
        .iter()
        .copied()
        .enumerate()
        .map(|(i, v)| (v, vec![(i + 1) as u32]))
        .collect::<HashMap<_, _>>();
    let mut val = *input.last().unwrap();
    for turn in (input.len() + 1)..=30000000 {
        if turn % 10000 == 0 {
            println!("{}", turn);
        }
        val = insert_next(&mut memory, turn as u32, val);
    }
    println!("{:?}", val);
}

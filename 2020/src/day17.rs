use std::collections::HashSet;
use std::fs::read_to_string;
use std::path::Path;

fn list_neighbors((r, c, z): (isize, isize, isize)) -> Vec<(isize, isize, isize)> {
    vec![
        (r - 1, c - 1, z - 1),
        (r, c - 1, z - 1),
        (r + 1, c - 1, z - 1),
        (r - 1, c, z - 1),
        (r, c, z - 1),
        (r + 1, c, z - 1),
        (r - 1, c + 1, z - 1),
        (r, c + 1, z - 1),
        (r + 1, c + 1, z - 1),
        (r - 1, c - 1, z),
        (r, c - 1, z),
        (r + 1, c - 1, z),
        (r - 1, c, z),
        (r + 1, c, z),
        (r - 1, c + 1, z),
        (r, c + 1, z),
        (r + 1, c + 1, z),
        (r - 1, c - 1, z + 1),
        (r, c - 1, z + 1),
        (r + 1, c - 1, z + 1),
        (r - 1, c, z + 1),
        (r, c, z + 1),
        (r + 1, c, z + 1),
        (r - 1, c + 1, z + 1),
        (r, c + 1, z + 1),
        (r + 1, c + 1, z + 1),
    ]
}

fn count_active_neighbors(
    state: &HashSet<(isize, isize, isize)>,
    pos: (isize, isize, isize),
) -> usize {
    list_neighbors(pos)
        .iter()
        .filter(|pos| state.contains(pos))
        .count()
}

enum Action {
    Activate((isize, isize, isize)),
    Deactivate((isize, isize, isize)),
}

fn get_deltas(state: &HashSet<(isize, isize, isize)>) -> Vec<Action> {
    state
        .iter()
        .flat_map(|pos| list_neighbors(*pos))
        .collect::<HashSet<_>>()
        .iter()
        .copied()
        .filter_map(|pos| {
            let ncount = count_active_neighbors(&state, pos);
            let is_active = state.contains(&pos);
            match is_active {
                true if ncount < 2 => Some(Action::Deactivate(pos)),
                true if 3 < ncount => Some(Action::Deactivate(pos)),
                false if ncount == 3 => Some(Action::Activate(pos)),
                _ => None,
            }
        })
        .collect::<Vec<_>>()
}

fn apply_deltas(
    mut state: HashSet<(isize, isize, isize)>,
    deltas: Vec<Action>,
) -> HashSet<(isize, isize, isize)> {
    deltas.iter().for_each(|a| {
        match *a {
            Action::Activate(pos) => state.insert(pos),
            Action::Deactivate(pos) => state.remove(&pos),
        };
    });
    state
}

pub fn day17(path: &Path) {
    let mut state = HashSet::new();
    read_to_string(path)
        .unwrap()
        .split("\n")
        .enumerate()
        .flat_map(|(irow, row)| {
            row.chars()
                .enumerate()
                .filter_map(move |(icol, val)| match val {
                    '.' => None,
                    '#' => Some((irow as isize, icol as isize)),
                    _ => panic!(),
                })
        })
        .for_each(|(irow, icol)| {
            state.insert((irow, icol, 0));
        });
    for _ in 0..6 {
        let deltas = get_deltas(&state);
        state = apply_deltas(state, deltas);
    }
    println!("{:?}", state);
}

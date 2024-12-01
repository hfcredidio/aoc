def maybe_get(grid, r, c, default="."):
    if r < 0 or c < 0 or r >= len(grid) or c >= len(grid[0]):
        return default
    return grid[r][c]


def count_occupied_neighbors1(grid, row, col):
    neighs = [
        (row + 1, col - 1),
        (row + 1, col),
        (row + 1, col + 1),
        (row, col - 1),
        (row, col + 1),
        (row - 1, col - 1),
        (row - 1, col),
        (row - 1, col + 1),
    ]
    return sum(state == "#" for r, c in neighs if (state := maybe_get(grid, r, c)))


def next_non_empty(grid, r, c, rstep, cstep):
    while True:
        r += rstep
        c += cstep
        state = maybe_get(grid, r, c, default="L")
        if state != ".":
            return state


def count_occupied_neighbors2(grid, row, col):
    neigh_states = [
        next_non_empty(grid, row, col, 1, -1),
        next_non_empty(grid, row, col, 1, 0),
        next_non_empty(grid, row, col, 1, 1),
        next_non_empty(grid, row, col, 0, -1),
        next_non_empty(grid, row, col, 0, 1),
        next_non_empty(grid, row, col, -1, -1),
        next_non_empty(grid, row, col, -1, 0),
        next_non_empty(grid, row, col, -1, 1),
    ]
    return sum(s == "#" for s in neigh_states)


def get_deltas(grid, count_neigh_fn, transition_fn):
    return [
        (r, c, nstate)
        for r, row in enumerate(grid)
        for c, state in enumerate(row)
        if state != "."
        and (nstate := transition_fn(state, count_neigh_fn(grid, r, c))) != state
    ]


def apply_deltas(grid, deltas):
    for r, c, s in deltas:
        grid[r][c] = s
    return grid


def transition1(state, ncount):
    return (
        "#"
        if state == "L" and ncount == 0
        else "L"
        if state == "#" and ncount >= 4
        else state
    )


def transition2(state, ncount):
    return (
        "#"
        if state == "L" and ncount == 0
        else "L"
        if state == "#" and ncount >= 5
        else state
    )


if __name__ == "__main__":
    with open("data/day11.txt") as fin:
        grid = [list(l.strip()) for l in fin]

    while True:
        deltas = get_deltas(grid, count_occupied_neighbors1, transition1)
        if not deltas:
            break
        grid = apply_deltas(grid, deltas)

    print(sum(row.count("#") for row in grid))

    with open("data/day11.txt") as fin:
        grid = [list(l.strip()) for l in fin]

    while True:
        deltas = get_deltas(grid, count_occupied_neighbors2, transition2)
        if not deltas:
            break
        grid = apply_deltas(grid, deltas)

    print(sum(row.count("#") for row in grid))

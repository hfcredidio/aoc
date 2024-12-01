def list_neigh_3d(r, c, z):
    return {
        (r - i, c - j, z - k)
        for i in range(-1, 2)
        for j in range(-1, 2)
        for k in range(-1, 2)
    } - {(r, c, z)}


def list_neigh_4d(r, c, z, w):
    return {
        (r - i, c - j, z - k, w - l)
        for i in range(-1, 2)
        for j in range(-1, 2)
        for k in range(-1, 2)
        for l in range(-1, 2)
    } - {(r, c, z, w)}


def step(state, list_neigh_fn):
    new_state = {i for i in state}
    extended_state = {n for p in state for n in list_neigh_fn(*p)} | state

    for p in extended_state:
        ncount = sum(n in state for n in list_neigh_fn(*p))
        if p in state and (ncount < 2 or 3 < ncount):
            new_state.remove(p)
        if p not in state and ncount == 3:
            new_state.add(p)
    return new_state


inp = """
....#...
.#..###.
.#.#.###
.#....#.
...#.#.#
#.......
##....#.
.##..#.#
""".strip()

state = {
    (i, j, 0)
    for i, line in enumerate(inp.split("\n"))
    for j, v in enumerate(line)
    if v == "#"
}

for i in range(6):
    state = step(state, list_neigh_3d)

print(f"3D count: {len(state)}")


inp = """
....#...
.#..###.
.#.#.###
.#....#.
...#.#.#
#.......
##....#.
.##..#.#
""".strip()
state = {
    (i, j, 0, 0)
    for i, line in enumerate(inp.split("\n"))
    for j, v in enumerate(line)
    if v == "#"
}
for i in range(6):
    state = step(state, list_neigh_4d)
print(f"4D count: {len(state)}")

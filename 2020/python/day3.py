def count_trees(file, vstep, hstep):
    col = -hstep
    res = sum(
        line[(col := (col + hstep) % len(line.strip()))] == "#"
        for i, line in enumerate(file)
        if i % vstep == 0
    )
    file.seek(0)
    return res


def day3():
    with open("data/day3.txt") as f:
        print(count_trees(f, 1, 3))
        print(
            count_trees(f, 1, 1) *
            count_trees(f, 1, 3) *
            count_trees(f, 1, 5) *
            count_trees(f, 1, 7) *
            count_trees(f, 2, 1)
        )

if __name__ == "__main__":
    day3()

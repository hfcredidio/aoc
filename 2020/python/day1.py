def twosum(arr, target):
    map = {}
    for i, v in enumerate(arr):
        residual = target - v
        if v in map:
            return map[v], i
        else:
            map[residual] = i


def threesum(arr, target):
    for i, v in enumerate(arr):
        residual = target - v
        if (res := twosum(arr, residual)) :
            return (i, *res)


def main():
    with open("data/day1.txt") as fin:
        vals = [int(i) for i in fin]

    i, j = twosum(vals, 2020)
    print(vals[i] * vals[j])

    i, j, k = threesum(vals, 2020)
    print(vals[i] * vals[j] * vals[k])


if __name__ == "__main__":
    main()

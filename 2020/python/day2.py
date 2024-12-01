import re

input_re = re.compile(f"([0-9]+)-([0-9]+) ([a-z]): ([a-z]+)")


def validate(min_count, max_count, letter, password):
    return min_count <= password.count(letter) <= max_count


def validate_toboggan(idx1, idx2, letter, password):
    return (password[idx1 - 1] == letter) != (password[idx2 - 1] == letter)


def parse_input(line):
    num1, num2, letter, password = input_re.match(line).groups()
    return int(num1), int(num2), letter, password


def main():
    with open("data/day2.txt") as fin:
        entries = [parse_input(line) for line in fin]

    print(sum(validate(*p) for p in entries))
    print(sum(validate_toboggan(*p) for p in entries))


if __name__ == "__main__":
    main()

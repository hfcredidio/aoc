import re


REQ_FIELDS = {"byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"}
EYE_COLORS = {"amb", "blu", "brn", "gry", "grn", "hzl", "oth"}


def parse_passport(data):
    return dict(e.split(":") for e in re.findall(r"[a-z]{3}:[a-z0-9#]+", data))


def passport_has_req_fields(pp):
    return all(f in pp for f in REQ_FIELDS)


def passport_data_is_valid(pp):
    height = maybe_int(pp["hgt"][:-2], 0)
    height_is_valid = (
        150 <= height <= 193
        if pp["hgt"].endswith("cm")
        else 59 <= height <= 76
        if pp["hgt"].endswith("in")
        else False
    )
    iget = lambda key: maybe_int(pp[key], default=0)
    return (
        re.match(r"^[0-9]{9}$", pp["pid"]) is not None
        and re.match(r"^#[0-9a-f]{6}$", pp["hcl"]) is not None
        and 1920 <= iget("byr") <= 2002
        and 2010 <= iget("iyr") <= 2020
        and 2020 <= iget("eyr") <= 2030
        and pp["ecl"] in EYE_COLORS
        and height_is_valid
    )


def maybe_int(x, default):
    return int(x) if x.isdecimal() else default


def day4():
    with open("data/day4.txt") as f:
        passports = [parse_passport(pp) for pp in f.read().split("\n\n")]
    print(sum(passport_has_req_fields(pp) for pp in passports))
    print(
        sum(
            passport_has_req_fields(pp) and passport_data_is_valid(pp)
            for pp in passports
        )
    )


if __name__ == "__main__":
    day4()

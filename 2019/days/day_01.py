import itertools as it
from typing import Callable, Iterable


def fuel_mass(mass: int) -> int:
    return (mass // 3) - 2


def iterate[T](f: Callable[[T], T], init: T) -> Iterable[T]:
    while True:
        yield init
        init = f(init)


def recursive_total_mass(total_mass: int) -> int:
    return sum(it.takewhile(lambda x: x > 0, iterate(fuel_mass, total_mass)))


if __name__ == "__main__":
    with open("data/day_01.txt") as file:
        masses = [int(line) for line in file]
    total_mass = sum(map(fuel_mass, masses))
    print(total_mass)
    correct_total_mass = sum(map(recursive_total_mass, masses)) - sum(masses)
    print(correct_total_mass)

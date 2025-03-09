from enum import Enum


class OpCode(Enum):
    ADD = 1
    MUL = 2
    HLT = 99


def eval_code(prog: list[int], pc: int) -> int | None:
    match OpCode(prog[pc]):
        case OpCode.ADD:
            lhs = prog[pc + 1]
            rhs = prog[pc + 2]
            res = prog[pc + 3]
            prog[res] = prog[lhs] + prog[rhs]
            return pc + 4
        case OpCode.MUL:
            lhs = prog[pc + 1]
            rhs = prog[pc + 2]
            res = prog[pc + 3]
            prog[res] = prog[lhs] * prog[rhs]
            return pc + 4
        case OpCode.HLT:
            return None


def eval_prog(prog: list[int], noun: int, verb: int) -> int:
    pc = 0
    prog = prog[:]
    prog[1] = noun
    prog[2] = verb
    while (pc_ := eval_code(prog, pc)) is not None:
        pc = pc_
    return prog[0]


if __name__ == "__main__":
    with open("data/day_02.txt") as file:
        prog = [int(i) for i in file.read().split(",")]
    print(eval_prog(prog, 12, 2))
    print(
        next(
            100 * noun + verb
            for noun in range(100)
            for verb in range(100)
            if eval_prog(prog, noun, verb) == 19690720
        )
    )

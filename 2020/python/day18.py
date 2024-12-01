"""gramar
prod := sum ('*' sum) *
sum := term ('+' term)*
term := LITERAL | '(' prod ')'

"""
from enum import Enum, auto
import re


class Token(Enum):
    NUMBER = auto()
    LPAR = auto()
    RPAR = auto()
    ADD = auto()
    MUL = auto()


class Node(Enum):
    SUM = auto()
    PROD = auto()
    TERM = auto()
    LITERAL = auto()


regexes = {
    Token.NUMBER: re.compile(r"[0-9]"),
    Token.LPAR: re.compile(r"\("),
    Token.RPAR: re.compile(r"\)"),
    Token.ADD: re.compile(r"\+"),
    Token.MUL: re.compile(r"\*"),
}


def tokenize(s):
    return [
        next(((t, w) for t, re in regexes.items() if re.match(w)))
        for w in s.replace(" ", "")
    ]


def parse_prod(tokens):
    children = [parse_sum(tokens)]
    while tokens and tokens[0][0] == Token.MUL:
        tokens.pop(0)
        children.append(parse_sum(tokens))
    return (Node.PROD, children)


def parse_sum(tokens):
    children = [parse_term(tokens)]
    while tokens and tokens[0][0] == Token.ADD:
        tokens.pop(0)
        children.append(parse_term(tokens))
    return (Node.SUM, children)


def parse_term(tokens):
    tok, val = tokens.pop(0)
    if tok == Token.NUMBER:
        return (Node.LITERAL, int(val))

    elif tok == Token.LPAR:
        node = (Node.TERM, [parse_prod(tokens)])
        if tokens[0][0] != Token.RPAR:
            raise SyntaxError
        tokens.pop(0)
        return node

    raise SyntaxError


def eval_node(node):
    typ, chil = node
    if typ == Node.LITERAL:
        return chil
    if typ == Node.SUM:
        return sum(eval_node(c) for c in chil)
    if typ == Node.PROD:
        res = 1
        for c in chil:
            res *= eval_node(c)
        return res
    if typ == Node.TERM:
        return eval_node(chil[0])
    raise Exception


plus = lambda a, b: a + b
mul = lambda a, b: a * b


def eval_expr(s):
    # part 1, the rest up there is part 2
    nstack = [0]
    ostack = [plus]

    for c in s:
        if c.isdigit():
            nstack[-1] = ostack[-1](int(c), nstack[-1])

        if c == "(":
            nstack.append(0)
            ostack.append(plus)

        if c == ")":
            ostack.pop()
            x = nstack.pop()
            nstack[-1] = ostack[-1](x, nstack[-1])

        if c == "+":
            ostack[-1] = plus

        if c == "*":
            ostack[-1] = mul

    return nstack[-1]


with open("data/day18.txt") as f:
    print(sum(eval_expr(line.strip()) for line in f))

with open("data/day18.txt") as f:
    print(sum(eval_node(parse_prod(tokenize(line.strip()))) for line in f))

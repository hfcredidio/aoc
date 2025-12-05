use_module(library(apply)).

direction(l).
direction(r).

read_lines(Filename, Lines) :-
    open(Filename, read, Stream),
    read_string(Stream, _Len, Content),
    close(Stream),
    split_string(Content, "\n", "\n", Lines).

parse_line(Line, Direction, Value) :-
    string_length(Line, Len),
    Len >= 2,
    string_chars(Line, [DirectionChar|ValChars]),
    atom_string(DirectionAtomU, DirectionChar),
    downcase_atom(DirectionAtomU, Direction),
    direction(Direction),
    atomics_to_string(ValChars, ValString),
    number_string(Value, ValString).

rotate_and_click(l, Value, Initial, Click, Final) :-
    Inter is Initial - Value,
    (Initial = 0 -> AddOne = 0; AddOne = 1),
    (Inter > 0 -> Click = 0; Click = AddOne - Inter // 100),
    Final is Inter mod 100.

rotate_and_click(r, Value, Initial, Click, Final) :-
    Click is (Initial + Value) // 100,
    Final is (Initial + Value) mod 100.

separate_results([], [], []).
separate_results([result(X, Y)|RS], [X|XS], [Y|YS]) :-
    separate_results(RS, XS, YS).

helper(Dir, Val, result(_, Initial), result(Click, Final)) :-
    rotate_and_click(Dir, Val, Initial, Click, Final).

rotate_multiple(Directions, Values, Initial, Clicks, Finals) :-
    scanl(helper, Directions, Values, result(0, Initial), Rotations),
    separate_results(Rotations, Clicks, Finals).

part1(InputFile, Solution) :-
    read_lines(InputFile, Lines),
    maplist(parse_line, Lines, Dirs, Values),
    rotate_multiple(Dirs, Values, 50, _, Positions),
    include(=(0), Positions, Zeros),
    length(Zeros, Solution).

part2(InputFile, Solution) :-
    read_lines(InputFile, Lines),
    maplist(parse_line, Lines, Dirs, Values),
    rotate_multiple(Dirs, Values, 50, Clicks, _),
    sum_list(Clicks, Solution).

test1(Solution) :- part1("day01/test_input.txt", Solution).
test2(Solution) :- part2("day01/test_input.txt", Solution).
solution1(Solution) :- part1("day01/input.txt", Solution).
solution2(Solution) :- part2("day01/input.txt", Solution).

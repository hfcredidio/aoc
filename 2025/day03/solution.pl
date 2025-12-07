read_lines(Filename, Lines) :-
    open(Filename, read, Stream),
    read_string(Stream, _Len, Content),
    close(Stream),
    split_string(Content, "\n", "\n", Lines).

parse_line(String, Numbers) :-
    string_chars(String, Chars),
    maplist(text_to_string, Chars, StringNums),
    maplist(number_string, Numbers, StringNums).

split_at(List, N, Left, Right) :-
    append(Left, Right, List),
    length(Left, N).

arg_max([X], 0, X).
arg_max([X|Rest], Idx, Max) :-
    arg_max(Rest, RestIdx, RestMax),
    (X >= RestMax ->
        Idx is 0, Max is X;
        Idx is RestIdx + 1, Max is RestMax).

max_split(List, Max, Rest) :-
    arg_max(List, Idx, Max),
    Idx_ is Idx + 1,
    split_at(List, Idx_, _, Rest).

jolt(1, Batteries, Acc, Jolt) :-
    max_list(Batteries, MaxBat),
    Jolt is Acc * 10 + MaxBat.

jolt(N, Batteries, Acc, Jolt) :-
    length(Batteries, NBatteries),
    Check is NBatteries - N + 1,
    split_at(Batteries, Check, BatteriesToCheck, UncheckedBatteries),

    max_split(BatteriesToCheck, MaxBat, CheckedBatteries),
    append(CheckedBatteries, UncheckedBatteries, RemainingBatteries),

    NextN is N - 1,
    NextAcc is 10 * Acc + MaxBat,
    jolt(NextN, RemainingBatteries, NextAcc, Jolt).

jolt(N, Batteries, Jolt) :- jolt(N, Batteries, 0, Jolt).

part1(Filename, Solution) :-
    read_lines(Filename, Lines),
    maplist(parse_line, Lines, BatteryList),
    maplist(jolt(2), BatteryList, Jolts),
    sum_list(Jolts, Solution).

part2(Filename, Solution) :-
    read_lines(Filename, Lines),
    maplist(parse_line, Lines, BatteryList),
    maplist(jolt(12), BatteryList, Jolts),
    sum_list(Jolts, Solution).

test1(Solution) :- part1("day03/test_input.txt", Solution).
test2(Solution) :- part2("day03/test_input.txt", Solution).
solution1(Solution) :- part1("day03/input.txt", Solution).
solution2(Solution) :- part2("day03/input.txt", Solution).

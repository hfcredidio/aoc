use_module(library(lists)).

read_lines(Filename, Lines) :-
    open(Filename, read, Stream),
    read_string(Stream, _Len, Content),
    close(Stream),
    split_string(Content, ",", "\n", Lines).

parse_line(Line, Start, End) :-
    split_string(Line, "-", "-", [StartStr, EndStr]),
    number_string(Start, StartStr),
    number_string(End, EndStr).

rdigits(0, []) :- !.
rdigits(N, [D|Digs]) :-
    D is N mod 10,
    NextN is N // 10,
    rdigits(NextN, Digs).

take(List, N, Taken, Rest) :-
    append(Taken, Rest, List),
    length(Taken, N).

chunk(List, 1, [List]).
chunk(List, N, [Chunk|Chunks]) :-
    length(List, L),
    between(1, L, N),
    L mod N =:= 0,
    ChunkSize is L // N,
    take(List, ChunkSize, Chunk, Rest),
    NextN is N - 1,
    chunk(Rest, NextN, Chunks).

all_unique([]).
all_unique([_]).
all_unique([X,X|XS]) :- all_unique([X|XS]).

is_invalid(Id, N) :-
    rdigits(Id, Digs),
    chunk(Digs, N, X),
    all_unique(X).

is_invalid(Id) :-
    findall(N, is_invalid(Id, N), Possible),
    length(Possible, L),
    L > 1.

find_invalids(N, Start, End, Invalids) :-
    findall(X, (between(Start, End, X), is_invalid(X, N)), Invalids).

find_invalids(Start, End, Invalids) :-
    findall(X, (between(Start, End, X), is_invalid(X)), Invalids).

part1(InputFile, Solution) :-
    read_lines(InputFile, Lines),
    maplist(parse_line, Lines, Starts, Ends),
    maplist(find_invalids(2), Starts, Ends, AllInvs),
    flatten(AllInvs, Invs),
    sum_list(Invs, Solution).

part2(InputFile, Solution) :-
    read_lines(InputFile, Lines),
    maplist(parse_line, Lines, Starts, Ends),
    maplist(find_invalids, Starts, Ends, AllInvs),
    flatten(AllInvs, Invs),
    sum_list(Invs, Solution).

test1(Solution) :- part1("day02/test_input.txt", Solution).
test2(Solution) :- part2("day02/test_input.txt", Solution).
solution1(Solution) :- part1("day02/input.txt", Solution).
solution2(Solution) :- part2("day02/input.txt", Solution).

#!/usr/bin/env -S swipl -O

:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).
:- use_module(library(pio)).

:- dynamic antenna/2.

:- initialization(main, main).
main(_) :-
    retractall(antenna(_, _)),
    phrase_from_stream(map(Map), user_input),
    places_to_antenna(Map, 0),
    length(Map, Dim), !,
    solve(part1, Dim, X1),
    format('Task 1: ~a~n', X1),
    solve(part2, Dim, X2),
    format('Task 2: ~a~n', X2).

% Task 1

solve(Part, Dim, X) :-
    setof(Pos, antinode(Part, Dim, Pos), Ps),
    length(Ps, X).

antinode(Part, Dim, X-Y) :-
    antenna(A, X1-Y1),
    antenna(A, X2-Y2),
    X1-Y1 \= X2-Y2,
    antinode_pos(Part, Dim, X1, Y1, X2, Y2, X, Y),
    X >= 0, Y >= 0, X < Dim, Y < Dim.

antinode_pos(part1, _, X1, Y1, X2, Y2, X, Y) :-
    (
        X is 2 * X1 - X2,
        Y is 2 * Y1 - Y2
    ;   X is 2 * X2 - X1,
        Y is 2 * Y2 - Y1
    ).

% Task 2

antinode_pos(part2, Dim, X1, Y1, X2, Y2, X, Y) :-
    DNeg is -Dim,
    between(DNeg, Dim, N),
    X is X1 + N * (X1 - X2),
    Y is Y1 + N * (Y1 - Y2).

% Parsing

map(Map) -->
    sequence(row, (blank, \+ eos), Map), blank.

row(Row) -->
    sequence(place, Row).

place(empty) --> `.`, !.
place(X) --> nonblank(X).

places_to_antenna([], _).
places_to_antenna([Row | Rows], Y) :-
    places_to_antenna(Row, Y, 0),
    Y1 is Y + 1,
    places_to_antenna(Rows, Y1).

places_to_antenna([], _, _).
places_to_antenna([Tile | Tiles], Y, X) :-
    ( Tile \= empty -> assertz(antenna(Tile, X-Y)) ; true ),
    X1 is X + 1,
    places_to_antenna(Tiles, Y, X1).

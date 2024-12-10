#!/usr/bin/env -S swipl -O

:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).
:- use_module(library(pio)).
:- use_module(library(clpfd)).

:- dynamic height_at/3.

:- initialization(main, main).
main(_) :-
    phrase_from_stream(map(Map), user_input),
    to_height_map(Map, 0),
    time(task1(X1)),
    format('Task 1: ~a~n', X1),
    time(task2(X2)),
    format('Task 2: ~a~n', X2).

% Task 1

task1(X) :-
    setof(P0-P9, trail(P0, P9), Ts),
    length(Ts, X).

trail(P0, P9) :-
    trail_path(trail(P0, _P1, _P2, _P3, _P4, _P5, _P6, _P7, _P8, P9)).

trail_path(trail(X0-Y0, P1, P2, P3, P4, P5, P6, P7, P8, X9-Y9)) :-
    height_at(X0, Y0, 0),
    adjacent_uphil(X0-Y0, P1),
    adjacent_uphil(P1, P2),
    adjacent_uphil(P2, P3),
    adjacent_uphil(P3, P4),
    adjacent_uphil(P4, P5),
    adjacent_uphil(P5, P6),
    adjacent_uphil(P6, P7),
    adjacent_uphil(P7, P8),
    adjacent_uphil(P8, X9-Y9),
    height_at(X9, Y9, 9).

:- table adjacent_uphil/2.
adjacent_uphil(X1-Y1, X2-Y2) :-
    height_at(X1, Y1, H1),
    H2 - H1 #= 1,
    (
        abs(X1 - X2) #= 1, Y1 = Y2
    ;   abs(Y1 - Y2) #= 1, X1 = X2
    ),
    height_at(X2, Y2, H2).

% Task 2

task2(X) :-
    setof(T, trail_path(T), Ts),
    length(Ts, X).

% Parsing

map(Map) --> sequence(row, (blank, \+ eos), Map), blank.
row(Row) --> sequence(height, Row).
height(H) -->
    digit(C), { number_chars(H, [C]) }.

to_height_map([], _).
to_height_map([Row | Rows], Y) :-
    to_height_map(Row, Y, 0),
    Y1 is Y + 1,
    to_height_map(Rows, Y1).

to_height_map([], _, _).
to_height_map([H | Hs], Y, X) :-
    assertz(height_at(X, Y, H)),
    X1 is X + 1,
    to_height_map(Hs, Y, X1).

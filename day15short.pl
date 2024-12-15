#!/usr/bin/env -S swipl -O

:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).
:- use_module(library(pio)).

:- use_module(map_utils).

task1(Map, Moves, Robot, X) :-
    foldl(move, Moves, Robot-Map, _-M1),
    score_map(M1, X).

move(Mov, R0-M0, Out) :-
    move_dir(Mov, R0, R1),
    \+ get_assoc(R1, M0, '#'),
    ( free_place(Mov, R1, M0, M1) -> Out = R1-M1 ; Out = R0-M0 ), !.
move(_, S, S).

free_place(_, Pos, M, M) :- get_assoc(Pos, M, '.'), !.
free_place(Mov, Pos, M0, M) :- move_stone(Mov, Pos, M0, M).

move_stone(Mov, Pos, M0, M) :-
    get_assoc(Pos, M0, What),
    What \= '#',
    put_assoc(Pos, M0, '.', M1),
    move_dir(Mov, Pos, Pos1),
    free_place(Mov, Pos1, M1, M2),
    put_assoc(Pos1, M2, What, M).

score_map(Map, X) :-
    aggregate_all(sum(S), score_tile(Map, S), X).

score_tile(Map, S) :-
    gen_assoc(Y-X, Map, T),
    ( T = 'O' -> S = 100 * Y + X ; S = 0 ).

% Parsing

from_file(Map, Moves, Robot) :-
    phrase_from_file(input(MapL, Moves), "day15.txt"),
    assoc_map(MapL, Map0),
    gen_assoc(Robot, Map0, '@'),
    put_assoc(Robot, Map0, '.', Map), !.

input(Map, Moves) -->
    map(Map), blank, blank, moves(Moves), blank.

moves(Moves) --> deep_chars(sequence(nonblanks, (blank, \+ eos)), M0), { flatten(M0, Moves) }.


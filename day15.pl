#!/usr/bin/env -S swipl -O

:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).
:- use_module(library(pio)).

:- initialization(main, main).
main(_) :-
    phrase_from_stream(input(Map, Moves, Robot), user_input), !,
    task1(Map, Moves, Robot, X1),
    format('Task 1: ~a~n', X1),
    task2(Map, Moves, Robot, X2),
    format('Task 2: ~a~n', X2).

map_bits(7).
map_width(X) :- map_bits(B), X is 1 << B - 1.
map_entries(X) :- map_bits(B), X is 1 << (2 * B).

% Task 1

task1(Map, Moves, Robot, X) :-
    foldl(move(Map), Moves, Robot, _),
    score_map(Map, X).

move(Map, M, R0, R) :-
    move_dir(M, R0, R), map_at(Map, R, empty), !.
move(Map, M, R0, R0) :-
    move_dir(M, R0, R), map_at(Map, R, wall), !.
move(Map, M, R0, R) :-
    % The robot tries to move into a stone
    move_dir(M, R0, R1),
    ( move_stone(Map, M, R1) -> R = R1 ; R = R0 ).

move_dir(up, X-Y, X-Y1) :- Y1 is Y - 1.
move_dir(down, X-Y, X-Y1) :- Y1 is Y + 1.
move_dir(left, X-Y, X1-Y) :- X1 is X - 1.
move_dir(right, X-Y, X1-Y) :- X1 is X + 1.

move_stone(Map, M, Pos) :-
    free_pos(Map, M, Pos, Pos1),
    set_map_at(Map, Pos, empty),
    set_map_at(Map, Pos1, stone(o)).

free_pos(Map, M, Pos0, Pos) :-
    move_dir(M, Pos0, Pos),
    map_at(Map, Pos, empty), !.
free_pos(Map, M, Pos0, Pos) :-
    move_dir(M, Pos0, Pos1),
    map_at(Map, Pos1, stone(_)),
    free_pos(Map, M, Pos1, Pos).

score_map(Map, X) :-
    aggregate_all(sum(S), score_tile(Map, S), X).

score_tile(Map, S) :-
    map_width(W),
    between(0, W, X),
    between(0, W, Y),
    map_at(Map, X-Y, T),
    (
        ground(T),
        ( T = stone(o) ; T = stone(right) )
    ->  S = 100 * Y + X
    ;   S = 0
    ).

% Task 2

task2(Map, Moves, X0-Y0, X) :-
    wide_map(Map, WMap),
    X1 is 2 * X0,
    foldl(move2(WMap), Moves, X1-Y0, _),
    score_map(WMap, X).

wide_map(Map, WMap) :-
    map_entries(Size),
    functor(WMap, map, Size),
    forall(coords(X, Y), dup_tile(X-Y, Map, WMap)).

coords(X, Y) :-
    map_bits(B),
    W is 1 << (B - 1) - 1,
    between(0, W, X),
    between(0, W, Y).

dup_tile(X-Y, Map, WMap) :-
    map_at(Map, X-Y, T),
    dup_tile_(WMap, X-Y, T).

dup_tile_(_, _, T) :- \+ ground(T), !.
dup_tile_(WMap, X-Y, stone(o)) :-
    X1 is 2 * X, X2 is X1 + 1,
    % a [] is stone(right), stone(left) to enable convenient finding of pairs
    nb_set_map_at(WMap, X1-Y, stone(right)),
    nb_set_map_at(WMap, X2-Y, stone(left)), !.
dup_tile_(WMap, X-Y, T) :-
    X1 is 2 * X, X2 is X1 + 1,
    nb_set_map_at(WMap, X1-Y, T),
    nb_set_map_at(WMap, X2-Y, T).

move2(Map, M, R0, R) :-
    move2_(Map, M, R0, R).

move2_(Map, M, R0, R) :-
    move_dir(M, R0, R), map_at(Map, R, empty), !.
move2_(Map, M, R0, R0) :-
    move_dir(M, R0, R), map_at(Map, R, wall), !.
move2_(Map, M, R0, R) :-
    % The robot tries to move into a stone
    move_dir(M, R0, R1),
    ( move_stone2(Map, M, R1) -> R = R1 ; R = R0 ).

move_stone2(Map, M, Pos) :-
    ( M = left ; M = right ),
    free_pos(Map, M, Pos, Pos1),
    set_map_at(Map, Pos, empty),
    move_dir(M, Pos, Pos2),
    set_stones_horiz(Map, Pos1, Pos2), !.
move_stone2(Map, M, Pos) :-
    % vertical movement
    move_dir(M, Pos, Pos1),
    \+ map_at(Map, Pos1, wall),
    map_at(Map, Pos, stone(LR)),
    set_map_at(Map, Pos, empty),
    (
        move_dir(LR, Pos, Pos2),
        map_at(Map, Pos2, stone(_))
    ->  move_stone2(Map, M, Pos2)
    ;   true
    ),
    (
        map_at(Map, Pos1, stone(_))
    ->  move_stone2(Map, M, Pos1)
    ;   true
    ),
    set_map_at(Map, Pos1, stone(LR)).

set_stones_horiz(Map, X0-Y, X1-Y) :-
    X0 > X1, !, set_stones_horiz(Map, X1-Y, X0-Y).
set_stones_horiz(Map, X0-Y, X1-Y) :-
    bagof(X-Y, between(X0, X1, X), Ps),
    lr_list(X0, X1, LRList),
    maplist(set_horiz_stone(Map), Ps, LRList).

lr_list(X0, X1, []) :- X1 < X0, !.
lr_list(X0, X1, [right, left | Rest]) :- X2 is X1 - 2, lr_list(X0, X2, Rest).

set_horiz_stone(Map, X-Y, LR) :- set_map_at(Map, X-Y, stone(LR)).

% Parsing

input(Map, Moves, Robot) -->
    map(MapList), blank, blank, moves(Moves), blank,
    { map_term(MapList, Map, Robot) }.

map(Map) -->
    sequence(row, (blank, \+ blank), Map).

row(Row) --> sequence(tile, Row).

tile(wall) --> `#` .
tile(empty) --> `.` .
tile(stone(o)) --> `O` .
tile(robot) --> `@` .

moves(M) -->
    sequence(moves_row, (blank, \+ eos), M0),
    { flatten(M0, M) }.
moves_row(M) --> sequence(move, M).

move(up) --> `^` .
move(down) --> `v` .
move(left) --> `<` .
move(right) --> `>` .

map_term(MapL, Map, Robot) :-
    map_entries(Size),
    functor(Map, map, Size),
    map_term(MapL, Map, Robot, 0).

map_term([], _, _, _).
map_term([Row | Rows], Map, Robot, Y) :-
    map_term(Row, Map, Robot, Y, 0),
    Y1 is Y + 1,
    map_term(Rows, Map, Robot, Y1).

map_term([], _, _, _, _).
map_term([L | Ls], Map, Robot, Y, X) :-
    (
        L = robot
    ->  Tile = empty,
        Robot = X-Y
    ;   Tile = L
    ),
    set_map_at(Map, X-Y, Tile),
    X1 is X + 1,
    map_term(Ls, Map, Robot, Y, X1).

map_at(Map, X-Y, T) :-
    map_coord(X-Y, N),
    arg(N, Map, T).

set_map_at(Map, X-Y, T) :-
    map_coord(X-Y, N),
    setarg(N, Map, T).

nb_set_map_at(Map, X-Y, T) :-
    map_coord(X-Y, N),
    nb_setarg(N, Map, T).

map_coord(X-Y, N) :- map_bits(B), N is Y << B + X + 1.

from_file(Map, Moves, Robot) :-
    phrase_from_file(input(Map, Moves, Robot), "day15.txt"), !.

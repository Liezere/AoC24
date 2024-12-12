#!/usr/bin/env -S swipl -O

:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).
:- use_module(library(pio)).

:- dynamic map_at/3.
:- dynamic region_at/3.

:- initialization(main, main).
main(_) :-
    phrase_from_stream(map(Map), user_input),
    assert_map_at(Map, 0),
    solve(part1, X1),
    format('Task 1: ~a~n', X1),
    solve(part2, X2),
    format('Task 2: ~a~n', X2).

% Task 1

solve(Part, X) :-
    retractall(region_at(_, _, _)),
    forall(map_regions, true),
    bagof([R, A, P], region_area_perimeter(Part, R, A, P), Rs),
    foldl(score_region, Rs, 0, X).

map_regions() :-
    nb_setval(max_region, 0),
    map_at(X, Y, L),
    \+ region_at(X, Y, _),
    new_region_at(X, Y, R),
    forall(flood_adjacent(X, Y, L, R), true).

flood_adjacent(X, Y, L, R) :-
    adjacent_coords(X, Y, X1, Y1),
    map_at(X1, Y1, L),
    \+ region_at(X1, Y1, _),
    assertz(region_at(X1, Y1, R)),
    forall(flood_adjacent(X1, Y1, L, R), true).

adjacent_region(X, Y, L, R) :-
    adjacent_coords(X, Y, X1, Y1),
    map_at(X1, Y1, L),
    region_at(X1, Y1, R), !.

adjacent_coords(X, Y, X1, Y1) :-
    (
        X1 is X - 1, Y1 = Y
    ;   X1 is X + 1, Y1 = Y
    ;   X1 = X, Y1 is Y - 1
    ;   X1 = X, Y1 is Y + 1
    ).

new_region_at(X, Y, R) :-
    nb_getval(max_region, R0),
    R is R0 + 1,
    nb_setval(max_region, R),
    assertz(region_at(X, Y, R)).

region_area_perimeter(Part, R, A, P) :-
    bagof(X-Y, region_at(X, Y, R), Coords),
    length(Coords, A),
    (
        Part = part1
    ->  maplist(xy_perimeter(R), Coords, Ps)
    ;   maplist(xy_sides(R), Coords, Ps)
    ),
    sum_list(Ps, P).

xy_perimeter(R, X-Y, P) :-
    aggregate_all(count, same_adjacent_region(X, Y, R), P).

same_adjacent_region(X, Y, R) :-
    adjacent_coords(X, Y, X1, Y1),
    \+ region_at(X1, Y1, R).

score_region([_, A, P], Acc0, Acc) :- Acc is Acc0 + A * P.

% Task 2

xy_sides(R, X-Y, P) :-
    aggregate_all(count, region_sides(X, Y, R), P).

region_sides(X, Y, R) :-
    adjacent_coords(X, Y, X1, Y1),
    side_between(X, Y, X1, Y1, R).

side_between(X1, Y, X2, Y, R) :-
    region_at(X1, Y, R),
    \+ region_at(X2, Y, R),
    Y0 is Y - 1,
    \+ same_side_between(X1, Y0, X2, Y0, R).
side_between(X, Y1, X, Y2, R) :-
    region_at(X, Y1, R),
    \+ region_at(X, Y2, R),
    X0 is X - 1,
    \+ same_side_between(X0, Y1, X0, Y2, R).

same_side_between(X, Y, X1, Y1, R) :-
    region_at(X, Y, R),
    \+ region_at(X1, Y1, R).

% Parsing

map(Map) --> sequence(row, (blank, \+ eos), Map), blank.
row(Row) --> sequence(nonblank, Row).

assert_map_at([], _).
assert_map_at([Row | Rows], Y) :-
    assert_map_at(Row, Y, 0),
    Y1 is Y + 1,
    assert_map_at(Rows, Y1).

assert_map_at([], _, _).
assert_map_at([L | Ls], Y, X) :-
    assertz(map_at(X, Y, L)),
    X1 is X + 1,
    assert_map_at(Ls, Y, X1).

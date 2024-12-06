#!/usr/bin/env -S swipl -O

:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).
:- use_module(library(pio)).

:- dynamic(map_at/3).
:- dynamic(visited/1).

:- initialization(main, main).
main(_) :-
    phrase_from_stream(map(Map), user_input), !,
    retractall(map_at(_, _, _)),
    places_to_map(Guard, Map, 0),
    task1(Guard, X1),
    format('Task 1: ~a~n', X1),
    task2(Guard, X2),
    format('Task 2: ~a~n', X2).

% Task 1

task1(guard(X-Y, Dir), L) :-
    retractall(visited(X-Y)),
    assertz(visited(X-Y)),
    move(X-Y, Dir, X1-Y1, Dir1),
    (
        off_map(X1-Y1) ->
        aggregate_all(count, visited(_), L)
    ;   task1(guard(X1-Y1, Dir1), L)
    ).

move(X-Y, Dir, X-Y, Dir1) :-
    next_pos(X-Y, Dir, X1-Y1),
    map_at(X1, Y1, obstacle),
    next_dir(Dir, Dir1), !.
move(X-Y, Dir, X1-Y1, Dir) :- next_pos(X-Y, Dir, X1-Y1).

next_pos(X-Y, up, X-Y1) :- Y1 is Y - 1.
next_pos(X-Y, down, X-Y1) :- Y1 is Y + 1.
next_pos(X-Y, left, X1-Y) :- X1 is X - 1.
next_pos(X-Y, right, X1-Y) :- X1 is X + 1.

next_dir(up, right).
next_dir(right, down).
next_dir(down, left).
next_dir(left, up).

off_map(X-Y) :- \+ map_at(X, Y, _).

% Task 2

task2(G, X) :-
    task1(G, _), /* aggregate visited */
    aggregate_all(count, loopy_obstacle(G, _), X).

loopy_obstacle(G, X-Y) :-
    visited(X-Y),
    \+ guard_at(G, X-Y),
    loopy_loop(G, X-Y).

guard_at(guard(P, _), P).

loopy_loop(G, Obs) :- empty_nb_set(V), loopy_loop(G, V, Obs).

loopy_loop(G, V, _) :- add_nb_set(G, V, false), !.
loopy_loop(guard(X-Y, Dir), V, Obs) :-
    add_nb_set(guard(X-Y, Dir), V),
    move2(X-Y, Dir, X1-Y1, Dir1, Obs),
    \+ off_map(X1-Y1),
    loopy_loop(guard(X1-Y1, Dir1), V, Obs).

move2(Pos, Dir, Pos, Dir1, Obs) :-
    move(Pos, Dir, Obs, _),
    next_dir(Dir, Dir1), !.
move2(Pos, Dir, Pos1, Dir1, _) :-
    move(Pos, Dir, Pos1, Dir1).

% Parsing

map(Map) -->
    sequence(row, (blank, \+ eos), Map), blank.

row(Row) -->
    sequence(place, Row).

place(empty) --> `.` .
place(obstacle) --> `#` .
place(guard(up)) --> `^` .
place(guard(down)) --> `v` .
place(guard(left)) --> `<` .
place(guard(right)) --> `>` .

places_to_map(_, [], _).
places_to_map(G, [Row | Rows], Y) :-
    places_to_map(G, Row, Y, 0),
    Y1 is Y + 1,
    places_to_map(G, Rows, Y1).

places_to_map(_, [], _, _).
places_to_map(G, [Tile | Tiles], Y, X) :-
    guard_pos(Tile, X, Y, G),
    tile_contents(Tile, At),
    assertz(map_at(X, Y, At)),
    X1 is X + 1,
    places_to_map(G, Tiles, Y, X1).

tile_contents(obstacle, obstacle) :- !.
tile_contents(_, empty).

guard_pos(guard(Dir), X, Y, guard(X-Y, Dir)) :- !.
guard_pos(_, _, _, _).

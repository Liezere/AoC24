#!/usr/bin/env -S swipl -O

:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).
:- use_module(library(pio)).
:- use_module(library(clpfd)).

:- use_module(library(func)).
:- use_module(library(list_util)).

:- use_module(utils).
:- use_module(map_utils).

task1(Ss, X) :-
    aggregate_all(count, matching_pair(Ss, _, _), X).

matching_pair(Ss, L, K) :-
    member([lock | L], Ss),
    member([key  | K], Ss),
    maplist(match, L, K).

match(P, R) :- P + R =< 5.

% Parsing

from_file(F, Ss) :-
    phrase_from_file(input(Ss), F).

input(Ss) -->
    sequence(schematic, (blank, blank), Ss), blank.

schematic([Typ | Hs]) -->
    map(Map),
    {
        ( nth1(1, nth1(1, Map, ~), ~) = '#' -> Typ = lock ; Typ = key ),
        map_heights(Map, [0, 0, 0, 0, 0], Hs0),
        maplist(minus_one, Hs0, Hs)
    }.

map_heights([], Hs, Hs).
map_heights([Row | Rows], Hs0, Hs) :-
    maplist(add_blank, Row, Hs0, Hs1),
    map_heights(Rows, Hs1, Hs).

add_blank(C, N0, N) :-
    ( C = '#' -> N1 = 1 ; N1 = 0 ),
    N is N0 + N1.

minus_one(X, X1) :- X1 is X - 1.

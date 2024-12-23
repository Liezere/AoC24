#!/usr/bin/env -S swipl -O

:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).
:- use_module(library(pio)).

:- use_module(utils).
:- use_module(map_utils).

task1(M, S, X) :-
    retractall(min_path_(_, _, _)),
    gen_path(S, M, P, LMax),
    bagof(L, cheat_path2(M, P, L), Ls),
    include(worthy_cheat(LMax), Ls, WLs),
    length(WLs, X).

worthy_cheat(LMax, L) :-
    L =< -100.

:- dynamic min_path_/3.

min_path(_, Y-X, L) :- min_path_(Y, X, L), !.
min_path(M, S, L) :-
    aggregate(min(L, P), gen_path(S, M, P, L), min(L, P)),
    forall(gen_assoc(Y-X, P, L1), debug_path(M, L, Y, X, L1)).

debug_path(M, LMax, Y, X, L) :-
    L1 is LMax - L + 1,
    assertz(min_path_(Y, X, L1)),
    %% format("Length: ~w~n", L1),
    %% draw_map(M, Y-X),
    true.

gen_path(S, M, P, L) :-
    empty_assoc(P0),
    put_assoc(S, P0, 0, P1),
    gen_path(S, M, P1, 0, L, P).

gen_path(S, M, P, L, L, P) :- get_assoc(S, M, 'E'), !.
gen_path(S, M, P0, L0, L, P) :-
    move_dir(_, S, S1),
    get_assoc(S1, M, T),
    ( T = '.' ; T = 'E' ),
    \+ get_assoc(S1, P0, _),
    L1 is L0 + 1,
    put_assoc(S1, P0, L1, P1),
    (
        T = 'E' -> P = P1, L = L1
    ;   gen_path(S1, M, P1, L1, L, P)
    ).

cheat_path(M, P, L) :-
    gen_assoc(S, P, L0),
    cheat_pos(M, S, S1),
    min_path(M, S1, L1),
    %% format("Cheat ~w(~w) -> ~w(~w)~n", [S, L0, S1, L1]),
    %% draw_map(M, S),
    L is L0 + L1 + 1.

cheat_path2(M, P, L) :-
    gen_assoc(Pl, P, 0),
    format("Paths: ~w~n", [Pl]),
    gen_assoc(S, P, L0),
    cheat_pos2(M, S, S1, CLen),
    gen_assoc(S1, P, L1),
    L is L0 + CLen - L1.


cheat_pos2(M, Y0-X0, Y-X, Len) :-
    XMin is X0 - 20, XMax is X0 + 20,
    YMin is Y0 - 20, YMax is Y0 + 20,
    between(XMin, XMax, X), between(YMin, YMax, Y),
    Len is abs(X - X0) + abs(Y - Y0), Len =< 20.

illegal_path(M, Y0-X0, Y1-X1) :-
    between(Y0, Y1, Y),
    between(X0, X1, X),
    get_assoc(Y-X, M, '#').

cheat_pos(M, S0, S2) :-
    move_dir(Dir, S0, S1),
    move_dir(Dir, S1, S2),
    get_assoc(S1, M, '#'),
    ( get_assoc(S2, M, '.') ; get_assoc(S2, M, 'E') ).

from_file(F, Map, Start) :-
    phrase_from_file(map(MapL), F),
    assoc_map(MapL, Map0),
    gen_assoc(Start, Map0, 'S'),
    put_assoc(Start, Map0, '.', Map).

#!/usr/bin/env -S swipl -O

:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).
:- use_module(library(pio)).

:- use_module(utils).
:- use_module(map_utils).

:- initialization(main, main).
main(_) :-
    read_problem(user_input, 71, 1024, M, Ps),
    task1(M, 70, X1),
    format('Task 1: ~a~n', X1),
    task2(M, 70, Ps, X2),
    format('Task 2: ~w~n', X2).

% Task 1

task1(M, Size, X) :-
    aggregate_all(min(Len), path_len(M, Size, Len), X).

path_len(M, Size, Len) :- path(M, Size, (Size-Size)-Path), length(Path, Len).

path(M, Size, VPath) :-
    list_to_assoc([(0-0)-1], Visited),
    empty_assoc(Lens),
    phrase(pathfind([(0-0)-[]], VPath), [s(M, Visited, Lens, Size)], [_]).

pathfind(Paths, VPath) -->
    state(s(_, _, _, End)),
    {    member(VPath, Paths),
         VPath = Pos-_-_,
         Pos = End }.
pathfind([P | Paths], VPath) -->
    state(s(M, Vis, Sc0, End), s(M, Vis1, Sc1, End)),
    {    findall(P1, next_path(M, Vis, P, P1), Paths1),
         append(Paths, Paths1, Paths2),
         foldl(update_scores, Paths1, Sc0, Sc1),
         include(best_score(Sc1), Paths2, Paths3),
         foldl(mark_visited, Paths1, Vis, Vis1) },
    pathfind(Paths3, VPath).

update_scores(Pos-Hist, Sc, Sc1) :-
    ( get_assoc(Pos, Sc, S) -> S0 = S ; S0 = inf ),
    length(Hist, S1),
    S1 < S0, put_assoc(Pos, Sc, S1, Sc1), !.
update_scores(_, S, S).

best_score(Sc, Pos-Hist) :-
    get_assoc(Pos, Sc, S0),
    length(Hist, S),
    S = S0.

next_path(M, V, P-Hist, P1-[P1 | Hist]) :-
    can_move(P, P1),
    get_assoc(P1, M, _), % inside map
    \+ get_assoc(P1, M, '#'),
    \+ get_assoc(P1, V, _).

mark_visited(P-_, V0, V) :- put_assoc(P, V0, 1, V).

can_move(P, P1) :- move_dir('>', P, P1).
can_move(P, P1) :- move_dir('v', P, P1).
can_move(P, P1) :- move_dir('<', P, P1).
can_move(P, P1) :- move_dir('^', P, P1).

% Task 2

task2(M, Size, Ps, X-Y) :-
    length(Pref, Size),
    append(Pref, Suff, Ps),
    failpos(M, Size, Suff, Y-X, []).

failpos(M, Size, [P | Ps], X, Hist) :-
    Hist \= [],
    \+ memberchk(P, Hist),
    put_assoc(P, M, '#', M1),
    failpos(M1, Size, Ps, X, Hist), !.
failpos(M, Size, [P | Ps], X, _) :-
    put_assoc(P, M, '#', M1),
    path(M1, Size, (Size-Size)-Hist), % path exists
    failpos(M1, Size, Ps, X, Hist), !.
failpos(_, _, [X | _], X, _).

% Parsing

read_problem(Stream, Size, Len, Map, Ps) :-
    phrase_from_stream(input(Ps), Stream),
    empty_assoc(Map0),
    Size2 is Size^2 - 1,
    bagof(N, between(0, Size2, N), Nums),
    foldl(empty_map(Size), Nums, Map0, Map1),
    length(Ps1, Len),
    append(Ps1, _, Ps),
    foldl(mark_byte, Ps1, Map1, Map).

empty_map(Size, N, M0, M) :-
    divmod(N, Size, Y, X),
    put_assoc(Y-X, M0, '.', M).

mark_byte(Y-X, M0, M) :- put_assoc(Y-X, M0, '#', M).

input(Ps) -->
    sequence(pos, blank1, Ps), blank.
pos(Y-X) -->
    integer(X), `,`, integer(Y).

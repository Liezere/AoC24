#!/usr/bin/env -S swipl -O

:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).
:- use_module(library(pio)).

:- use_module(utils).
:- use_module(map_utils).

solve(M, Pos, X1, X2) :-
    bagof(Path, path(M, Pos, Path), Ps),
    aggregate_all(min(S), score_path(Ps, S), X1),
    empty_assoc(Vis0),
    length(Ps, PL),
    foldl(visit_best(X1), Ps, Vis0, Vis),
    assoc_to_keys(Vis, KL),
    length(KL, KLL),
    X2 is KLL + 1.

visit_best(SMin, _-_-Hist, V0, V) :-
    score(Hist, S),
    SMin = S,
    foldl(mark_best, Hist, V0, V), !.
visit_best(_, _, V, V).

mark_best(mov(P, _), V0, V) :- put_assoc(P, V0, 1, V).

score_path(Ps, S) :-
    member(P, Ps),
    P = _-_-Hist,
    score(Hist, S).

path(M, Pos, VPath) :-
    list_to_assoc([Pos-1], Visited),
    empty_assoc(Scores),
    phrase(pathfind([Pos-'>'-[]], VPath), [s(M, Visited, Scores)], [_]).
    
pathfind(Paths, VPath) -->
    state(s(M, _, _)),
    {    member(VPath, Paths),
         VPath = Pos-_-_,
         get_assoc(Pos, M, 'E') }.
pathfind([P | Paths], VPath) -->
    state(s(M, Vis, Sc), s(M, Vis1, Sc1)),
    {    findall(P1, next_path(M, Vis, P, P1), Paths1),
         foldl(update_scores, Paths1, Sc, Sc1),
         append(Paths, Paths1, Paths2),
         include(best_score(Sc1), Paths2, Paths3),
         pairs_keys(Paths3, DP),
         P = DPos-_,
         foldl(mark_visited, Paths1, Vis, Vis1) },
    pathfind(Paths3, VPath).

update_scores(Pos-_-Hist, Sc, Sc1) :-
    ( get_assoc(Pos, Sc, S) -> S0 = S ; S0 = inf ),
    score(Hist, S1),
    S1 < S0, put_assoc(Pos, Sc, S1, Sc1), !.
update_scores(_, S, S).

best_score(Sc, Pos-_-Hist) :-
    get_assoc(Pos, Sc, S0),
    score(Hist, S),
    ( S = S0 ; S is S0 + 1000 ).

next_path(M, V, P-D-Hist, P1-D1-[mov(P1, Dc) | Hist]) :-
    can_move(P, D, P1, D1),
    \+ get_assoc(P1, M, '#'),
    \+ get_assoc(P1, V, _),
    ( D = D1 -> Dc = '=' ; Dc = D1 ).

mark_visited(P-_, V0, V) :- put_assoc(P, V0, 1, V).

can_move(P, D, P1, D) :- move_dir(D, P, P1).
can_move(P, D, P1, D1) :- rotate(D, D1), move_dir(D1, P, P1).

rotate('>', D) :- ( D = '^' ; D = 'v').
rotate('<', D) :- ( D = '^' ; D = 'v').
rotate('^', D) :- ( D = '<' ; D = '>').
rotate('v', D) :- ( D = '<' ; D = '>').

score(Ms, S) :- score(Ms, S, 0).
score([], S, S).
score([mov(_, D) | Ms], S, S0) :-
    ( D = '=' -> Add = 1 ; Add = 1001 ),
    S1 is S0 + Add,
    score(Ms, S, S1).

from_file(Map, Start) :-
    phrase_from_file(map(MapL), "day16.txt"),
    assoc_map(MapL, Map0),
    gen_assoc(Start, Map0, 'S'),
    put_assoc(Start, Map0, '.', Map).

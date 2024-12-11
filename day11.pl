#!/usr/bin/env -S swipl -O

:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).

solve(Ns, Depth, X) :-
    maplist(stone_count(Depth), Ns, Counts),
    foldl(plus, Counts, 0, X), !.

:- table stone_count/3.
stone_count(0, _, 1) :- !.
stone_count(D, 0, N) :- D1 is D - 1, stone_count(D1, 1, N), !.
stone_count(D, S, N) :-
    number_codes(S, Ds),
    length(Ds, NDigit),
    divmod(NDigit, 2, Half, 0),
    length(Pref, Half),
    append(Pref, Suff, Ds),
    % --
    D1 is D - 1,
    number_codes(S1, Pref),
    stone_count(D1, S1, N1),
    number_codes(S2, Suff),
    stone_count(D1, S2, N2),
    N is N1 + N2, !.
stone_count(D, S, N) :-
    D1 is D - 1,
    S1 is S * 2024,
    stone_count(D1, S1, N).

input(DataStr, Ns) :- phrase(sequence(integer, ` `, Ns), DataStr).

#!/usr/bin/env -S swipl -O

:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).
:- use_module(library(pio)).

:- use_module(utils).

% Task 1
task1(Ns, X) :-
    maplist(evolve_n(2000), Ns, Es),
    sum_list(Es, X).

evolve_n(T, N0, N) :-
    dotimes(T, evolve, N0, N).

evolve(N0, N) :-
    N1 is N0 << 6,
    mixprune(N0, N1, N2),
    N3 is N2 >> 5,
    mixprune(N2, N3, N4),
    N5 is N4 << 11,
    mixprune(N4, N5, N).

mixprune(N0, N1, N) :-
    N2 is N0 xor N1,
    N is N2 mod 16777216.

% Task 2
task2(Ns, X) :-
    empty_assoc(A0),
    foldl(diffs_to_sum, Ns, A0, A),
    aggregate_all(max(B, D), gen_assoc(D, A, B), max(X, _)).

diffs_to_sum(N, A0, A) :-
    empty_assoc(S0),
    evolve_seq(2000, N, Ps),
    foldl(diff_to_first, Ps, S0, S),
    assoc_to_list(S, Sl),
    foldl(diff_to_sum, Sl, A0, A), !.

diff_to_first(D-B, A0, A) :-
    ( get_assoc(D, A0, _) -> A = A0 ; put_assoc(D, A0, B, A) ).

diff_to_sum(D-B, A0, A) :-
    ( get_assoc(D, A0, B0) -> B1 is B0 ; B1 = 0 ),
    B2 is B1 + B,
    put_assoc(D, A0, B2, A).

evolve_seq(L, N0, Ps) :-
    L1 is L - 1,
    dotimes(L1, accum_evolve(N0), [], RNs),
    reverse(RNs, Ns),
    maplist(mod10, [N0 | Ns], MNs),
    difflist(MNs, Ds),
    L2 is L - 4,
    length(MNs1, L2),
    append(_, MNs1, MNs),
    diffprices(Ds, MNs1, Ps).

accum_evolve(N0, [], [N]) :- evolve(N0, N).
accum_evolve(_, [N0 | Rest], [N, N0 | Rest]) :- evolve(N0, N).

mod10(X, Y) :- Y is X mod 10.

difflist([_], []).
difflist([X, Y | Xs], [D | Ds]) :-
    D is Y - X,
    difflist([Y | Xs], Ds).

diffprices(_, [], []).
diffprices([A, B, C, D | Ds], [P | Ps], [dl(A, B, C, D)-P | DPs]) :-
    diffprices([B, C, D | Ds], Ps, DPs).

% Parsing

from_file(F, Ns) :-
    phrase_from_file((sequence(integer, blank1, Ns), blank), F).

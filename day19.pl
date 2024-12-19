#!/usr/bin/env -S swipl -O

:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).
:- use_module(library(pio)).

:- use_module(utils).

% Task1
task1(Ts, Gs, X) :-
    aggregate(count, assemble(Ts, Gs), X).

assemble(Ts, Gs) :-
    member(G, Gs),
    match(G, Ts).

match([], _) :- !.
match(G, Ts) :-
    member(T, Ts),
    append(T, Rest, G),
    match(Rest, Ts), !.

% Task 2
task2(Ts, Gs, X) :-
    aggregate(sum(C), total_matches(Ts, Gs, C), X).

total_matches(Ts, Gs, C) :-
    member(G, Gs),
    count_matches(Ts, G, C).

:- table count_matches/3.
count_matches(_, [], 1).
count_matches(Ts, G, C) :-
    convlist(single_match(Ts, G), Ts, Cs),
    foldl(plus, Cs, 0, C).

single_match(Ts, G, T, C) :-
    append(T, Rest, G),
    count_matches(Ts, Rest, C).

% Parsing
from_file(F, Ts, Gs) :-
    phrase_from_file(input(Ts, Gs), F).

input(Ts, Gs) -->
    sequence(string, `, `, Ts), blank, blank, sequence(string, blank1, Gs), blank.

#!/usr/bin/env -S swipl -O

:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).
:- use_module(library(pio)).

:- initialization(main, main).
main(_) :-
    phrase_from_stream(input(Eqs), user_input), !,
    solve(part1, Eqs, X1),
    format('Task 1: ~a~n', X1),
    solve(part2, Eqs, X2),
    format('Task 2: ~a~n', X2).

% Task 1

solve(Part, Eqs, X) :-
    convlist(has_solution(Part), Eqs, GdEqs),
    aggregate_all(sum(G), member(G, GdEqs), X).

has_solution(Part, eq(Goal, Nums), Goal) :-
    reverse(Nums, Nums1),
    sol(Part, eq(Goal, Nums1), Goal).

sol(_, eq(Num, [Num]), Num) :- !.
sol(P, eq(Goal, [Num | Nums]), Goal) :- sol(P, eq(G0, Nums), G0), Goal is Num + G0.
sol(P, eq(Goal, [Num | Nums]), Goal) :- sol(P, eq(G0, Nums), G0), Goal is Num * G0.

% Task 2

sol(part2, eq(Goal, [Num | Nums]), Goal) :-
    sol(part2, eq(G0, Nums), G0),
    atomic_list_concat([G0, Num], X),
    atom_number(X, Goal).

% Parsing

input(Eqs) -->
    sequence(equation, (blank, \+ eos), Eqs), blank.

equation(eq(Goal, Nums)) -->
    number(Goal), `: `, sequence(number, ` `, Nums).

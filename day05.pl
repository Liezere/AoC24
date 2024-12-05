#!/usr/bin/env -S swipl -O

:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).
:- use_module(library(pio)).

:- dynamic(after/2).

:- initialization(main, main).
main(_) :-
    phrase_from_stream(input(Rules, Pages), user_input),
    assert_after(Rules),
    task1(Pages, X1),
    format('Task 1: ~a~n', X1),
    task2(Pages, X2),
    format('Task 2: ~a~n', X2).

% Task 1

task1(Pages, X) :-
    convlist(correct_order, Pages, GdPages),
    sum_mid_pages(GdPages, X).

correct_order(Pages, Pages) :-
    forall(subseq(Pages, [X, Y], _), after(X, Y)).

sum_mid_pages(Pages, X) :-
    maplist(midpage, Pages, MPs),
    foldl(plus, MPs, 0, X).

midpage(Pages, E) :-
    length(Pages, N),
    Mid is (N + 1) / 2,
    nth1(Mid, Pages, E).

% Task 2

task2(Pages, X) :-
    convlist(bad_order, Pages, BadPages),
    maplist(fix_order, BadPages, GdPages),
    sum_mid_pages(GdPages, X).

bad_order(Pages, Pages) :- \+ correct_order(Pages, Pages).

fix_order(P, P) :- correct_order(P, P), !.
fix_order(P0, P) :-
    append([Prefix, [X, Y], Suffix], P0),
    \+ after(X, Y),
    append([Prefix, [Y, X], Suffix], P1),
    fix_order(P1, P), !.

% Parse input

input(Rules, Pages) -->
    sequence(rule, (blank, \+ blank), Rules),
    blank, blank,
    sequence(sequence(integer, `,`), (blank, \+ eos), Pages),
    blank.

rule(X-Y) --> integer(X), `|`, integer(Y).

assert_after(Rules) :-
    forall(member(X-Y, Rules), assertz(after(X, Y))).

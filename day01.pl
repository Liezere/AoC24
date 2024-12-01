#!/usr/bin/env -S swipl -O

:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).
:- use_module(library(pio)).

:- initialization(main, main).

main(_) :-
    task_inputs(user_input, Xs, Ys),
    task1(Xs, Ys, X),
    format('Task 1: ~a~n', X),
    task2(Xs, Ys, Y),
    format('Task 2: ~a~n', Y).

% Task 1

task1(Xs, Ys, X) :-
    msort(Xs, Xss),
    msort(Ys, Yss),
    foldl(diffsum, Xss, Yss, 0, X).

diffsum(X, Y, S0, S) :-
    S is S0 + abs(X - Y).

% Task 2

task2(Xs, Ys, X) :-
    msort(Ys, Yss),
    clumped(Yss, Yc),
    score2(Xs, Yc, X).

score2(Xs, Yc, X) :- score2(Xs, Yc, 0, X).
score2([], _, S, S).
score2([X | Xs], Yc, S0, S) :-
    (
        memberchk(X-C, Yc),
        S1 is S0 + X * C
    ;   S1 = S0
    ),
    !,
    score2(Xs, Yc, S1, S).

% Parse inputs

task_inputs(S, Xs, Ys) :-
    phrase_from_stream(task_input(Pairs), S),
    split_inputs(Pairs, Xs, Ys).

task_input(Pairs) -->
    sequence(row, (blank, \+ eos), Pairs), blank.

row(X-Y) -->
    integer(X), blank, blank, blank, integer(Y).

split_inputs([], [], []).
split_inputs([X-Y | Pairs], [X | Xs], [Y | Ys]) :-
    split_inputs(Pairs, Xs, Ys).

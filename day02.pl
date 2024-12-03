#!/usr/bin/env -S swipl -O

:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).
:- use_module(library(pio)).

:- use_module(library(clpfd)).

:- initialization(main, main).

main(_) :-
    task_inputs(user_input, Reports),
    task1(Reports, N1),
    format('Task 1: ~a~n', N1),
    task2(Reports, N2),
    format('Task 2: ~a~n', N2).

% Task 1
task1(Reports, N) :-
    convlist(safe, Reports, SafeReps),
    length(SafeReps, N).

safe([_, _], 0).
safe([X, Y, Z | Rest], 0) :-
    abs(X - Y) #=< 3,
    abs(Y - Z) #=< 3,
    ( X #< Y, Y #< Z ; X #> Y, Y #> Z ),
    safe([Y, Z | Rest], 0).

% Task 2
task2(Reports, N) :-
    convlist(dsafe, Reports, SafeReps),
    length(SafeReps, N).

dsafe(Ls, 0) :-
    select(_, Ls, Dampened),
    safe(Dampened, 0),
    !.

% Parse inputs
task_inputs(S, Reports) :-
    phrase_from_stream(task_input(Reports), S).

task_input(Reports) -->
    sequence(report, (blank, \+ eos), Reports), blank.

report(R) -->
    sequence(integer, ` `, R).

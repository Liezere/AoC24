#!/usr/bin/env -S swipl -O

:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).
:- use_module(library(pio)).

:- use_module(library(clpfd)).

:- initialization(main, main).
main(_) :-
    phrase_from_stream(machines(Ms), user_input),
    solve(part1, Ms, X1),
    format('Task 1: ~a~n', X1),
    solve(part2, Ms, X2),
    format('Task 2: ~a~n', X2).

% Task 1/2

solve(Part, Ms, X) :-
    convlist(play_min(Part), Ms, SuccPlays),
    foldl(score_play, SuccPlays, 0, X).

% The cut ensures minimum score
play_min(Part, M, Ta-Tb) :- play(Part, M, Ta, Tb), !.

play(Part, machine(Ax-Ay, Bx-By, Gx-Gy), Ta, Tb) :-
    ( Part = part2 -> Cor = 10000000000000 ; Cor = 0 ),
    Ta in 0 .. sup,
    Tb in 0 .. sup,
    Cor + Gx #= Ax * Ta + Bx * Tb,
    Cor + Gy #= Ay * Ta + By * Tb,
    indomain(Ta), indomain(Tb).

score_play(Ta-Tb, Acc0, Acc) :- Acc is Acc0 + 3 * Ta + Tb.

% Parsing

from_file(Ms) :-
    phrase_from_file(machines(Ms), "day13.txt").

machines(Ms) -->
    sequence(machine, (blank, blank), Ms), blank.

machine(machine(Ax-Ay, Bx-By, Gx-Gy)) -->
    `Button A: X+`, integer(Ax), `, Y+`, integer(Ay), blank,
    `Button B: X+`, integer(Bx), `, Y+`, integer(By), blank,
    `Prize: X=`, integer(Gx), `, Y=`, integer(Gy).

    

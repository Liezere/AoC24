#!/usr/bin/env -S swipl -O

:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).
:- use_module(library(pio)).

:- use_module(library(clpfd)).

:- initialization(main, main).
main([InFile]) :-
    phrase_from_file(robots(R), InFile),
    task1(R, 101, 103, 100, X1),
    task2(R, X2),
    format('Task 1: ~a~n', X1),
    format('Task 2: ~a~n', X2).

% Task 1

task1(R, Dx, Dy, Steps, X) :-
    maplist(advance(Dx, Dy, Steps), R, R1),
    foldl(count_q(Dx, Dy), R1, qcount(0, 0, 0, 0), qcount(A, B, C, D)),
    X is A * B * C * D.

advance(Dx, Dy, Steps, robot(X-Y, Vx-Vy), robot(X1-Y1, Vx-Vy)) :-
    X1 is (X + Steps * Vx) mod Dx,
    Y1 is (Y + Steps * Vy) mod Dy.

count_q(Dx, Dy, robot(X-Y, _), qcount(A, B, C, D), qcount(A1, B, C, D)) :-
    X < Dx div 2, Y < Dy div 2, A1 is A + 1, !.
count_q(Dx, Dy, robot(X-Y, _), qcount(A, B, C, D), qcount(A, B1, C, D)) :-
    X > Dx div 2, Y < Dy div 2, B1 is B + 1, !.
count_q(Dx, Dy, robot(X-Y, _), qcount(A, B, C, D), qcount(A, B, C1, D)) :-
    X < Dx div 2, Y > Dy div 2, C1 is C + 1, !.
count_q(Dx, Dy, robot(X-Y, _), qcount(A, B, C, D), qcount(A, B, C, D1)) :-
    X > Dx div 2, Y > Dy div 2, D1 is D + 1, !.
count_q(_, _, _, C, C).

% Task 2

task2(R, N) :-
    format('Press ENTER until you see a cluster, then press SPACE+ENTER~n'),
    wait_for_input([user_input], _, infinite), get_char('\n'),
    visual_search(R, 101, 0, X),
    visual_search(R, 103, 0, Y),
    Dim is 101 * 103,
    N in 0 .. sup,
    N #= (X * 101 + Y * 103) mod Dim,
    indomain(N),
    maplist(advance(101, 103, N), R, R1),
    draw(101, 103, R1), !.

visual_search(R, Period, N, N1) :-
    draw(101, 103, R),
    maplist(advance(101, 103, Period), R, R1),
    wait_for_input([user_input], _, infinite),
    (
        get_char('\n')
    ->  N0 is N + 1,
        visual_search(R1, Period, N0, N1)
    ;   get_char(_), N1 = N
    ).

draw(Dx, Dy, R) :- draw(Dx, Dy, R, 0, 0).
draw(_, Dy, _, _, Dy) :- put('\n'), put('\n'), !.
draw(Dx, Dy, R, Dx, Y) :- Y1 is Y + 1, put('\n'), draw(Dx, Dy, R, 0, Y1), !.
draw(Dx, Dy, R, X, Y) :-
    ( member(robot(X-Y, _), R) -> put('#') ; put('.') ),
    X1 is X + 1,
    draw(Dx, Dy, R, X1, Y), !.

% Parsing

robots(Rs) --> sequence(robot, (blank, \+ eos), Rs), blank.

robot(robot(X-Y, Vx-Vy)) -->
    `p=`, integer(X), `,`, integer(Y), ` v=`, integer(Vx), `,`, integer(Vy).

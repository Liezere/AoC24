#!/usr/bin/env -S swipl -O

:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).
:- use_module(library(pio)).

:- use_module(library(clpfd)).

:- dynamic(letter/2).

:- initialization(main, main).
main(_) :-
    phrase_from_stream((sequence(nonblanks, (blank, \+ eos), In), blank), user_input),
    input_letters(In, 0),
    aggregate_all(count, xmas_pos(_, _, _, _), X1),
    format("Task 1: ~a~n", X1),
    aggregate_all(count, x_mas_pos(_, _, _, _, _), X2),
    format("Task 2: ~a~n", X2).

% Parse to DB

input_letters([], _).
input_letters([Row | Rows], X) :-
    input_letters(Row, X, 0),
    X1 is X + 1,
    input_letters(Rows, X1).
input_letters([], _, _).
input_letters([L | Ls], X, Y) :-
    char_code(C, L),
    assertz(letter(X-Y, C)),
    Y1 is Y + 1,
    input_letters(Ls, X, Y1).

% Task 1

xmas_pos(A-I, B-J, C-K, D-L) :-
    (
        monotone(A, B, C, D), fixed(I, J, K, L)
    ;   fixed(A, B, C, D), monotone(I, J, K, L)
    ;   monotone(A, B, C, D), monotone(I, J, K, L)
    ),
    letter(A-I, 'X'), letter(B-J, 'M'), letter(C-K, 'A'), letter(D-L, 'S').

monotone(A, B, C, D) :-
    ( Diff = 1 ; Diff = -1 ),
    A - B #= Diff, B - C #= Diff, C - D #= Diff.

fixed(X, X, X, X).

% Task 2

x_mas_pos(A-I, A-K, B-J, C-I, C-K) :-
    B - A #= 1, C - B #= 1, J - I #= 1, K - J #= 1,
    letter(B-J, 'A'),
    (
        letter(A-I, 'M'), letter(C-K, 'S')
    ;   letter(A-I, 'S'), letter(C-K, 'M')
    ),
    (
        letter(C-I, 'M'), letter(A-K, 'S')
    ;   letter(C-I, 'S'), letter(A-K, 'M')
    ).

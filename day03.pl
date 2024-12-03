#!/usr/bin/env -S swipl -O

:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).

:- initialization(main, main).

main(_) :-
    read_stream_to_codes(user_input, Str),
    task1(Str, X1),
    format('Task 1: ~a~n', X1),
    task2(Str, X2),
    format('Task 2: ~a~n', X2).

% Task 1

task1(Str, X) :-
    phrase(mul_ops(Ops), Str), !,
    foldl(mulpair, Ops, 0, X).

mul_ops(Ops) -->
    sequence(mul_op, Ops), string(_).

mul_op(X-Y) -->
    string(_), `mul(`, number(X), `,`, number(Y), `)`.

mulpair(X-Y, Acc, R) :- R is Acc + X * Y.

% Task 2

task2(Str, X) :-
    % Append do() to the end to ensure valid_prefixes matches all don't's
    append(Str, `do()`, Str1),
    phrase(valid_prefixes(Prefs, Tail), Str1),
    flatten(Prefs, Head),
    append(Head, Tail, Input), !,
    task1(Input, X).

valid_prefixes(Xs, Tail) -->
    sequence(valid_prefix, Xs), string(Tail).

valid_prefix(X) -->
    string(X), `don't()`, string(_), `do()`.

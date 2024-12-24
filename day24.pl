#!/usr/bin/env -S swipl -O

:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).
:- use_module(library(pio)).
:- use_module(library(clpfd)).

:- use_module(library(func)).
:- use_module(library(list_util)).

:- use_module(utils).
:- use_module(map_utils).

:- dynamic val/2.

% Task 1
task1(F, X) :-
    from_file(F, Is, Gs),
    to_prolog(Is, Gs),
    bagof(Bit-Val, sol_bit(Bit, Val), Bs),
    foldl(bitsum, Bs, 0, X).

bitsum(B-V, S0, S) :-
    S is S0 + (V << B).

sol_bit(Bit, X) :-
    atom_codes(val(~, X), [char_code(z, ~) | Ncs]),
    number_codes(Bit, Ncs).

to_prolog(Is, Gs) :-
    retractall(val(_, _)),
    forall(member(I, Is), base_val(I)),
    forall(member(G, Gs), calc_val(G)).

base_val(Id-N) :- assertz(val(Id, N)).
calc_val(g(and, I1, I2, O)) :- assertz((val(O, X) :- val(I1, X1), val(I2, X2), X #= X1 * X2)).
calc_val(g(or, I1, I2, O)) :- assertz((val(O, X) :- val(I1, X1), val(I2, X2), X #= max(X1, X2))).
calc_val(g(xor, I1, I2, O)) :- assertz((val(O, X) :- val(I1, X1), val(I2, X2), X #= X1 xor X2)).

% Task 2

%% Thread through assuming every bit is calculated as
%% xi xor yi -> bi
%% xp and yp -> pi
%% cp or pi -> ri
%% ri xor bi -> zi
%% ri and bi -> ci
%% If the assumption fails just try to substitue Rn/Bn/Zn with something that could work.

task2(F) :-
    from_file(F, _, Gs),
    check_op(Gs, and, x00, y00, C0),
    check_op(Gs, and, C0, _, C1),
    thread(2, C1, Gs, false, []).

check_op(Gs, Op, Arg1, Arg2, Out) :-
    ( member(g(Op, Arg1, Arg2, Out), Gs) ; member(g(Op, Arg2, Arg1, Out), Gs) ).

thread(45, _, _, _, Fixes) :- print_fixes(sort $ Fixes), !.
thread(D, CPrev, Gs, IsFix, Fixes) :-
    xyz_d(D, Xn, Yn, Zn),
    xyz_d(~ is D - 1, Xp, Yp, _),
    check_op(Gs, xor, Xn, Yn, Bn),
    check_op(Gs, and, Xp, Yp, Pn),
    check_op(Gs, or, CPrev, Pn, Rn),
    (
        check_op(Gs, xor, Rn, Bn, Zn)
    ->  check_op(Gs, and, Rn, Bn, Cn),
        thread(~ is D + 1, Cn, Gs, false, Fixes)
    ;   fix(IsFix, Gs, Rn, Bn, Zn, W1-W2),
        swap_wire(W1, W2, Gs, Gs1),
        thread(D, CPrev, Gs1, false, [W1, W2 | Fixes]), !
    ).

print_fixes(Fixes) :-format("Swaps: ~s~n", [split(~, 0',, maplist(atom_codes, Fixes, ~))]).

xyz_d(D, Xn, Yn, Zn) :-
    atom_codes(Xn, `x~48t~d~3+` $ D),
    atom_codes(Yn, `y~48t~d~3+` $ D),
    atom_codes(Zn, `z~48t~d~3+` $ D).

fix(false, Gs, Rn, Bn, Zn, Zn-Bad) :- check_op(Gs, xor, Rn, Bn, Bad).
fix(false, Gs, Rn, Bn, Zn, Bn-Bad) :- check_op(Gs, xor, Rn, Bad, Zn).
fix(false, Gs, Rn, Bn, Zn, Rn-Bad) :- check_op(Gs, xor, Bad, Bn, Zn).

swap_wire(O1, O2, Gs0, Gs) :-
    select(g(A1, B1, C1, O1), Gs0, g(A1, B1, C1, mark), Gs1),
    select(g(A2, B2, C2, O2), Gs1, g(A2, B2, C2, O1), Gs2),
    select(g(A3, B3, C3, mark), Gs2, g(A3, B3, C3, O2), Gs).

% Parsing
    
from_file(F, Is, Gs) :-
    phrase_from_file(inputs(Is, Gs), F).

inputs(Is, Gs) -->
    sequence(input, blank1, Is), blank, blank,
    sequence(gate, blank1, Gs), blank.

input(Id-N) -->
    string(Ids), `: `, digit(Ns),
    { atom_codes(Id, Ids), number_codes(N, [Ns]) }.

gate(g(Op, I1, I2, O)) -->
    string(I1s), ` `, oper(Op), ` `, string(I2s), ` -> `, string(Os),
    { atom_codes(I1, I1s), atom_codes(I2, I2s), atom_codes(O, Os) }.

oper(and) --> `AND`.
oper(or) --> `OR`.
oper(xor) --> `XOR`.
    

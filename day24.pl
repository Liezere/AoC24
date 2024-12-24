#!/usr/bin/env -S swipl -O

:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).
:- use_module(library(pio)).
:- use_module(library(clpfd)).

:- use_module(library(func)).
:- use_module(library(lambda)).

:- use_module(utils).
:- use_module(map_utils).

:- dynamic val/2.

% Task 1
task1(Is, Gs, X) :-
    to_prolog(Is, Gs),
    bagof(Bit-Val, sol_bit(Bit, Val), Bs),
    foldl(bitsum, Bs, 0, X).

bitsum(B-V, S0, S) :-
    S is S0 + (V << B).

sol_bit(Bit, X) :-
    val(Id, X),
    atom_codes(Id, [char_code(z, ~) | Ncs]),
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

%% Thread through assuming
%% xi xor yi -> bi
%% xp and yp -> pi
%% cp or pi -> ri
%% ri xor bi -> zi
%% ri and bi -> ci

%% Part 2 is solved just by threading until failure, manually adding a swap and retrying.h

thread(D, CPrev, Gs) :-
    xyz_d(D, Xn, Yn, Zn),
    xyz_d(~ is D - 1, Xp, Yp, _),
    format("R~d ~4|", [D]),
    ( member(g(xor, Xn, Yn, Bn), Gs) ; member(g(xor, Yn, Xn, Bn), Gs) ),
    format("B: ~w  ", [Bn]),
    ( member(g(and, Xp, Yp, Pn), Gs) ; member(g(and, Yp, Xp, Pn), Gs) ),
    format("P: ~w  ", [Pn]),
    ( member(g(or, CPrev, Pn, Rn), Gs) ; member(g(or, Pn, CPrev, Rn), Gs) ),
    format("R: ~w  ", [Rn]),
    ( member(g(xor, Rn, Bn, Zn), Gs) ; member(g(xor, Bn, Rn, Zn), Gs) ),
    format("Z: ~w  ", [Zn]),
    ( member(g(and, Rn, Bn, Cn), Gs) ; member(g(and, Bn, Rn, Cn), Gs) ),
    format("C: ~w~n", [Cn]),
    thread(~ is D + 1, Cn, Gs).

xyz_d(D, Xn, Yn, Zn) :-
    D >= 10,
    atom_codes(Xn, `x~d` $ D),
    atom_codes(Yn, `y~d` $ D),
    atom_codes(Zn, `z~d` $ D), !.
xyz_d(D, Xn, Yn, Zn) :-
    atom_codes(Xn, `x0~d` $ D),
    atom_codes(Yn, `y0~d` $ D),
    atom_codes(Zn, `z0~d` $ D).

fix(Gs0, Gs) :-
    swap_wire(z06, fkp, Gs0, Gs1),
    swap_wire(z11, ngr, Gs1, Gs2),
    swap_wire(krj, bpt, Gs2, Gs3),
    swap_wire(z31, mfm, Gs3, Gs).

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
    

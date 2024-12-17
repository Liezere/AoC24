#!/usr/bin/env -S swipl -O

:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).
:- use_module(library(pio)).

:- use_module(utils).

:- initialization(main, main).
main(_) :-
    phrase_from_stream(input(A, _, _, Prog), user_input),
    task1(Prog, A), % prints its own output
    task2(Prog, X2),
    format('Task 2: ~a~n', X2).

% Task 1
task1(Prog, A) :-
    execute(Prog, A, Out),
    phrase(sequence(integer, `,`, Out), Str),
    format('Task1: ~s~n', [Str]).

execute(Prog, A, Out) :-
    phrase(repeatedly(exec), [s(Prog, A, 0, 0, []) | Prog], [s(_, _, _, _, Out0)]), !,
    reverse(Out0, Out).

repeatedly(P) --> P, !, repeatedly(P).
repeatedly(_) --> [].

exec --> instr(0, V    ), regA(A0, A), { A is A0 // (2 ^ V) }.
exec --> instr(1, V    ), regB(B0, B), { B is B0 xor V }.
exec --> instr(2, V    ), regB(_, B), { B is V mod 8 }.
exec --> instr(3, _    ), regA(0, 0), !.
exec --> instr(3, Addr ), jump(Addr).
exec --> instr(4, _    ), regB(B0, B), regC(C, C), { B is B0 xor C }.
exec --> instr(5, V0   ), out(V), {V is V0 mod 8}.
exec --> instr(6, V    ), regA(A, A), regB(_, B), { B is A // (2 ^ V) }.
exec --> instr(7, V    ), regA(A, A), regC(_, C), { C is A // (2 ^ V) }.

instr(Op, Oper) --> instr_raw(Op, Raw), oper_val(Raw, Oper).
instr_raw(Op, Raw), [S] --> [S, Op, Raw].
oper_val(Raw, V) -->
    state(s(_, A, B, C, _)),
    {(   Raw = 4 -> V = A
     ;   Raw = 5 -> V = B
     ;   Raw = 6 -> V = C
     ;   V = Raw )}.
regA(A0, A) --> state(s(P, A0, B, C, Out), s(P, A, B, C, Out)).
regB(B0, B) --> state(s(P, A, B0, C, Out), s(P, A, B, C, Out)).
regC(C0, C) --> state(s(P, A, B, C0, Out), s(P, A, B, C, Out)).
out(X) --> state(s(P, A, B, C, Out0), s(P, A, B, C, [X | Out0])).
jump(Addr), [s(P, A, B, C, Out) | P1] -->
    [s(P, A, B, C, Out) | _], eos,
    {   append(Pref, P1, P),
        length(Pref, Addr) }.

% Task 2
task2(Prog, A) :- fixpoint(Prog, 0, 14, A), !.

fixpoint(Prog, A, -1, A) :- execute(Prog, A, Prog).
fixpoint(Prog, A0, Ord, A) :-
    Ord >= 0,
    between(0, 63, Mask),
    A1 is A0 xor (Mask << (3 * Ord)),
    execute(Prog, A1, Out),
    MatchLen is 16 - Ord,
    length(Suffix, MatchLen),
    append(_, Suffix, Prog),
    append(_, Suffix, Out),
    Ord1 is Ord - 1,
    fixpoint(Prog, A1, Ord1, A).

% Parsing
input(A, B, C, Prog) -->
    `Register A: `, integer(A), blank,
    `Register B: `, integer(B), blank,
    `Register C: `, integer(C), blank,
    blank,
    `Program: `, sequence(integer, `,`, Prog), blank.

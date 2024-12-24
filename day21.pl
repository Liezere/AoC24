#!/usr/bin/env -S swipl -O

:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).
:- use_module(library(pio)).

:- use_module(utils).
:- use_module(map_utils).

:- dynamic numpad/2, dirpad/2.

task1(C, D, X) :-
    retractall(numpad(_, _)),
    retractall(dirpad(_, _)),
    assert_np(), assert_dp(),
    maplist(decode_np, C, C1),
    maplist(deep_count(D), C1, Ls),
    maplist(code_num, C, Ns),
    maplist(mul, Ls, Ns, Ss),
    foldl(plus, Ss, 0, X).

mul(A, B, C) :- C is A * B.

code_num(C, N) :-
    append(Np, [_], C),
    number_chars(N, Np).

solve_short(D, C, L) :-
    aggregate(min(L, P), solve_len(D, C, P, L), min(L, Out)),
    format("~w -> ~s~n", [C, Out]).

solve_len(D, C, Out, L) :-
    solve(D, C, Out),
    length(Out, L).

solve(D, C, Out) :-
    repeat(D, dp(0-2), Dps),
    State = [np(3-2) | Dps],
    solve(C, State, Out, _).

repeat(0, El, []) :- !.
repeat(N, El, [El | Rest]) :- N1 is N - 1, repeat(N1, El, Rest).


solve([], S, [], S) :- !.
solve(C, [], C, []) :- !.
solve([C | Cs], S0, Out, S) :-
    solve1(C, S0, Out0, S1),
    solve(Cs, S1, Out1, S),
    append(Out0, Out1, Out).

solve1(_, [], [], []) :- !.
solve1(C, [S0 | Ss0], Out, [S | Ss]) :-
    solve2(C, S0, Out0, S),
    append(Out0, ['A'], Out1),
    solve(Out1, Ss0, Out, Ss).

solve2(C, S0, Out, S) :-
    S0 =.. [Pad, Pos0],
    clumpy_move(Pad, Pos0, '<', C, Out, Pos),
    S =.. [Pad, Pos].


clumpy_move(Pad, Pos0, C0, C, Out, Pos) :-
    aggregate_all(min(Cl), move_to(Pad, Pos0, C0, C, _, _, Cl), Cl1), !,
    move_to(Pad, Pos0, C0, C, Out, Pos, Cl1).

move_to(Pad, Pos0, C0, C, Out, Pos, Clumpiness) :-
    move_to_(Pad, Pos0, C, Out, Pos, []),
    sort(Out, Unique),
    length(Unique, L),
    L =< 2,
    clumpiness(Out, 0, Clumpiness).

clumpiness([], _, 0).
clumpiness([_], C, C).
clumpiness(['<', '^' | Rest], C0, C) :-
    C1 is C0 + 0.5,
    clumpiness(['^' | Rest], C1, C), !.
clumpiness(['v', '>' | Rest], C0, C) :-
    C1 is C0 + 0.5,
    clumpiness(['>' | Rest], C1, C), !.
clumpiness([X, Y | Rest], C0, C) :-
    ( X = Y -> C1 = 0 ; C1 = 1 ),
    C2 is C0 + C1,
    clumpiness([Y | Rest], C2, C).

move_to_(Pad, Pos, C, [], Pos, _) :- getpad(Pad, Pos, C), !.
move_to_(Pad, Pos0, C, [M | Ms], Pos, Vis) :-
    getpad(Pad, Pos1, D),
    \+ member(D, Vis),
    move_dir(M, Pos0, Pos1),
    move_to_(Pad, Pos1, C, Ms, Pos, [D | Vis]).

deep_move(Pad, Pos0, C0, C, Out, Pos) :-
    aggregate(min(Cl, Out-Pos), double_clump(Pad, Pos0, C0, C, Out, Pos, Cl), min(_, Out-Pos)).

double_clump(Pad, Pos0, C0, C, Out, Pos, Cl) :-
    clumpy_move(Pad, Pos0, C0, C, Out, Pos),
    solve(Out, [dp(0-2), dp(0-2)], Out1, _),
    format("Try: ~s -> ~s -> ~s~n", [C, Out, Out1]),
    length(Out1, Cl).

getpad(np, P, C) :- numpad(P, C).
getpad(dp, P, C) :- dirpad(P, C).

% Task 2
deep_count(D, C, N) :- deep_count(D, '.', '.', C, 0-2, 0-2, _, _, N).

:- table deep_count/9.
deep_count(0, _, _, Cs, _, _, 'A', 0-2, N) :- length(Cs, N), !.
deep_count(_, _, O0, [], _, PosO, O0, PosO, 0) :- !.
deep_count(D, C0, O0, [C | Cs], Pos, PosO, ON, PosON, N) :-
    clumpy_move(dp, Pos, C0, C, Out0, Pos1),
    ( append(_, [Cx], Out0) -> C1 = Cx ; C1 = C0 ),
    append(Out0, ['A'], Out1),
    D1 is D - 1, !,
    deep_count(D1, O0, '.', Out1, PosO, 0-2, O1, PosO1, N1),
    deep_count(D, C1, O1, Cs, Pos1, PosO1, ON, PosON, N2),
    N is N1 + N2.

decode_np(C, Out) :- decode(np, '.', C, 3-2, Out), !, format("~s~n", [Out]).
decode_dp(C, Out) :- decode(dp, '.', C, 0-2, Out), !, format("~s~n", [Out]).
decode(_, _, [], _, []).
decode(Pad, C0, [C | Cs], Pos, Out) :-
    clumpy_move(Pad, Pos, C0, C, Out0, Pos1),
    ( append(_, [Cx], Out0) -> C1 = Cx ; C1 = C0 ),
    append(Out0, ['A'], Out1),
    decode(Pad, C1, Cs, Pos1, Out2),
    append(Out1, Out2, Out).

assert_np() :-
    phrase(map(Pad), `789 456 123 X0A`),
    assoc_map(Pad, PadM),
    forall(gen_assoc(Y-X, PadM, Btn), assertz(numpad(Y-X, Btn))),
    retractall(numpad(_, 'X')).
assert_dp() :-
    phrase(map(Pad), `X^A <v>`),
    assoc_map(Pad, PadM),
    forall(gen_assoc(Y-X, PadM, Btn), assertz(dirpad(Y-X, Btn))),
    retractall(dirpad(_, 'X')).


from_file(F, Codes) :-
    phrase_from_file(codes(Codes), F).

codes(Codes) -->
    deep_chars(sequence(nonblanks, blank1), Codes), blank.
% <A       >A   <A       A v   <  A    A >>  ^   A  vA     A ^A   <    vA    AA ^  >   A
% v<<A>>^A vA^A v<<A>>^A A <vA <A >>^A A vAA ^<A >A <vA^>A A <A>A v<<A >A>^A AA <A >vA ^A

% <A       >A   <AA       v<AA       >>^A     vAA     ^A   <vAAA       ^>A
% v<<A>>^A vA^A v<<A>>^AA <vA<A>>^AA vAA^<A>A <vA^>AA <A>A v<<A>A>^AAA <A >vA ^A

% ^A            ^^        <<         A        >>      A    vvv         A
% <A       >A   <AA       v<AA       >>^A     vAA     ^A   v<AAA       ^>A
% v<<A>>^A vA^A v<<A>>^AA v<A<A>>^AA vAA^<A>A v<A^>AA <A>A v<A<A>>^AAA <A>vA^A


% ^A            <<          ^^       A        >>      A    vvv         A
% <A       >A   v<<AA       >^AA     >A       vAA     ^A   <vAAA       ^>A
% <v<A>>^A vA^A <vA<AA>>^AA vA<^A>AA vA^A     <vA^>AA <A>A <v<A>A^>AAA <A>vA^A

% 1st bad
% <A                  ^^^A            >vvA                 vA
% <A                  ^^^A            vv>A                 vA
% v<<A       >>^A     <AAA       >A   <vAA       >A   ^A   <vA       ^>A
% <vA<AA>>^A vAA<^A>A v<<A>>^AAA vA^A v<<A>A^>AA vA^A <A>A v<<A>A^>A <A>vA^A

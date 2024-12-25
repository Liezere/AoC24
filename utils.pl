:- module(utils, [ blank1//0, state//1, state//2, dotimes/4 ]).

:- use_module(library(dcg/basics)).

blank1 --> blank, \+ blank, \+ eos.

state(S), [S] --> [S].
state(S0, S), [S] --> [S0].

dotimes(0, _, S, S) :- !.
dotimes(N, P, S0, S) :-
    call(P, S0, S1),
    N1 is N - 1,
    dotimes(N1, P, S1, S).

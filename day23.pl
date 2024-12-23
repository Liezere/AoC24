#!/usr/bin/env -S swipl -O

:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).
:- use_module(library(pio)).

:- use_module(utils).
:- use_module(map_utils).

% Task 1
task1(M, X) :-
    setof(P, lan_party(M, P), Ps0),
    include(has_t, Ps0, Ps),
    length(Ps, X).

has_t([X, Y, Z]) :-
    ( nth1(1, X, 116) ; nth1(1, Y, 116) ; nth1(1, Z, 116) ), !.

lan_party(M, P) :-
    gen_assoc(X, M, Ys),
    member(Y, Ys),
    gen_assoc(Y, M, Zs),
    member(Z, Zs),
    get_assoc(Z, M, Xs),
    member(X, Xs),
    sort([X, Y, Z], P).

% Task 2
task2(M) :-
    setof(P, lan_party(M, P), Ps0),
    convlist(lan_party_x(M), Ps0, Ps1),
    map_list_to_pairs(length, Ps1, Ps2),
    sort(1, @>, Ps2, Ps3),
    forall(member(_-X, Ps3), format_comp(X)).

format_comp([X]) :- format("~s~n", [X]), !.
format_comp([X | Xs]) :- format("~s,", [X]), format_comp(Xs).

lan_party_x(M, [X | Xs], P) :-
    get_assoc(X, M, Ys),
    member(Y, Ys),
    \+ member(Y, Xs),
    at_party(M, Y, Xs),
    ( lan_party_x(M, [Y, X | Xs], P1) -> P2 = P1 ; P2 = [Y, X | Xs] ),
    sort(P2, P).

at_party(_, _, []).
at_party(M, X, [X | Rest]) :- !, at_party(M, X, Rest).
at_party(M, X, [Y | Rest]) :-
    get_assoc(Y, M, Ys),
    memberchk(X, Ys),
    at_party(M, X, Rest).

from_file(F, M) :-
    phrase_from_file(sequence(link, Ls), F), !,
    link_map(Ls, M).

link_map(Ls, M) :- empty_assoc(M0), link_map(Ls, M0, M).
link_map([], M, M).
link_map([X-Y | Rest], M0, M) :-
    add_map(X, M0, Y, M1),
    add_map(Y, M1, X, M2),
    link_map(Rest, M2, M).

add_map(X, M0, Y, M) :-
    ( get_assoc(X, M0, Ys0) -> Ys = Ys0 ; Ys = [] ),
    put_assoc(X, M0, [Y | Ys], M).

link(X-Y) -->
    string(X), `-`, string(Y), blank.

:- module(map_utils, [map//1, deep_chars//2, deep_chars/2, assoc_map/2, draw_map/2, move_dir/3, enumerate0/2]).

map(Map) --> deep_chars(sequence(nonblanks, (blank, \+ blank, \+ eos)), Map).

deep_chars(Element, X) --> call(Element, X0), { deep_chars(X0, X) }.

deep_chars([], []).
deep_chars([X | Xs], [C | Cs]) :- deep_chars(X, C), deep_chars(Xs, Cs), !.
deep_chars(X, C) :- char_code(C, X).

assoc_map(List, Map) :-
    maplist(enumerate0, List, EXList),
    enumerate0(EXList, EList),
    maplist(liftpairs, EList, PPList),
    flatten(PPList, PList),
    list_to_assoc(PList, Map).

enumerate0(X, Y) :- enumerate0(X, Y, 0).

enumerate0([], [], _).
enumerate0([X | Xs], [N-X | EXs], N) :- N1 is N + 1, enumerate0(Xs, EXs, N1).

liftpairs(Y-Xs, Ps) :-
    maplist(liftpair(Y), Xs, Ps).

liftpair(Y, X-V, (Y-X)-V).

draw_map(Map, AtAt) :-
    assoc_to_list(Map, Pairs),
    maplist(draw_tile(AtAt), Pairs),
    put_char('\n'),
    wait_for_input([user_input], _, infinite),
    get_char('\n').

draw_tile(AtAt, Y-X-T) :-
    ( X = 0 -> nl ; true ),
    ( AtAt = Y-X -> put_char('@') ; put_char(T) ).

move_dir('^', Y-X, Y1-X) :- Y1 is Y - 1.
move_dir('v', Y-X, Y1-X) :- Y1 is Y + 1.
move_dir('<', Y-X, Y-X1) :- X1 is X - 1.
move_dir('>', Y-X, Y-X1) :- X1 is X + 1.

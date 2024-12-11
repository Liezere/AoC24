#!/usr/bin/env -S swipl -O

solve_iter(Ns, Depth, X) :-
    maplist(init_one, Ns, InitCounts),
    list_to_assoc(InitCounts, D0),
    length(Iter, Depth),
    foldl(split_stones, Iter, D0, D),
    assoc_to_values(D, Counts),
    sum_list(Counts, X).

init_one(S, S-1).

split_stones(_, D0, D1) :-
    assoc_to_list(D0, L0),
    empty_assoc(DNew),
    foldl(split_stone, L0, DNew, D1).

split_stone(0-N, D0, D) :- add_assoc(D0, 1, N, D), !.
split_stone(S-N, D0, D) :-
    number_codes(S, Ds),
    length(Ds, NDigit),
    divmod(NDigit, 2, Half, 0),
    length(Pref, Half),
    append(Pref, Suff, Ds),
    % --
    number_codes(S1, Pref),
    add_assoc(D0, S1, N, D1),
    number_codes(S2, Suff),
    add_assoc(D1, S2, N, D), !.
split_stone(S-N, D0, D) :- S1 is S * 2024, add_assoc(D0, S1, N, D).

add_assoc(D0, S, N, D) :-
    ( get_assoc(S, D0, Val) -> N0 = Val ; N0 = 0 ),
    N1 is N + N0,
    put_assoc(S, D0, N1, D).

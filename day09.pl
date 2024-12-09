#!/usr/bin/env -S swipl -O

:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).
:- use_module(library(pio)).

:- initialization(main, main).
main(_) :-
    phrase_from_stream((digits(Raw), blank), user_input),
    maplist(plus(-48), Raw, List),
    task1(List, X1),
    format('Task 1: ~a~n', X1),
    task2(List, X2),
    format('Task 2: ~a~n', X2).

% Task 1

task1(List, X) :-
    length(List, Len),
    sum_list(List, MaxIdx),
    Data =.. [data | List],
    MaxLen is 9 * Len,
    functor(Disk, disk, MaxLen),
    forall(between(1, MaxLen, Idx), nb_setarg(Idx, Disk, 0)),
    disk_write(Disk, Data, 1, 1),
    disk_compact(Disk, 1, MaxIdx),
    disk_score(Disk, 1, X).

disk_write(Disk, Data, Idx, FIdx) :-
    arg(FIdx, Data, N),
    Idx1 is Idx + N - 1,
    FNum is FIdx div 2,
    forall(between(Idx, Idx1, I), nb_setarg(I, Disk, FNum)),
    Idx2 is Idx1 + 1,
    FIdx1 is FIdx + 1,
    disk_blank(Disk, Data, Idx2, FIdx1).

disk_blank(_, Data, _, FIdx) :-
    \+ arg(FIdx, Data, _), !.
disk_blank(Disk, Data, Idx, FIdx) :-
    arg(FIdx, Data, N),
    Idx1 is Idx + N - 1,
    forall(between(Idx, Idx1, I), nb_setarg(I, Disk, blank)),
    Idx2 is Idx1 + 1,
    FIdx1 is FIdx + 1,
    disk_write(Disk, Data, Idx2, FIdx1).

disk_compact(_Disk, Idx, RIdx) :- Idx > RIdx, !.
disk_compact(Disk, Idx, RIdx) :-
    \+ arg(Idx, Disk, blank), !,
    Idx1 is Idx + 1,
    disk_compact(Disk, Idx1, RIdx).
disk_compact(Disk, Idx, RIdx) :-
    arg(RIdx, Disk, blank), !,
    nb_setarg(RIdx, Disk, 0),
    RIdx1 is RIdx - 1,
    disk_compact(Disk, Idx, RIdx1).
disk_compact(Disk, Idx, RIdx) :-
    arg(RIdx, Disk, V),
    nb_setarg(Idx, Disk, V),
    nb_setarg(RIdx, Disk, 0),
    Idx1 is Idx + 1,
    RIdx1 is RIdx - 1,
    disk_compact(Disk, Idx1, RIdx1).

disk_score(Disk, Idx, X) :- disk_score(Disk, Idx, 0, X).

disk_score(Disk, Idx, X, X) :- \+ arg(Idx, Disk, _), !.
disk_score(Disk, Idx, Acc, X) :-
    arg(Idx, Disk, Val),
    Acc1 is Acc + Val * (Idx - 1),
    Idx1 is Idx + 1,
    disk_score(Disk, Idx1, Acc1, X).

% Task 2

task2(List, X) :-
    to_index_list(0, 0, List, Files, Blanks),
    reverse(Files, RFiles),
    BlankArr =.. [blanks | Blanks],
    FileArr =.. [files | RFiles],
    compact_files(FileArr, BlankArr),
    FileArr =.. [files | CompFiles],
    maplist(score_file, CompFiles, FileScores),
    foldl(plus, FileScores, 0, X).

to_index_list(Idx, FIdx, [F], [file(FIdx, Idx, F)], []) :- !.
to_index_list(Idx, FIdx, [F, B | Rest], [file(FIdx, Idx, F) | FRest], [IdxB-B | BRest]) :-
    IdxB is Idx + F,
    Idx1 is IdxB + B,
    FIdx1 is FIdx + 1,
    to_index_list(Idx1, FIdx1, Rest, FRest, BRest).

compact_files(Files, Blanks) :-
    compact_files(Files, Blanks, 1, lenat(1, 1, 1, 1, 1, 1, 1, 1, 1)).

compact_files(Files, _Blanks, Idx, _MinLen) :-
    \+ arg(Idx, Files, _), !.
compact_files(Files, Blanks, Idx, MinLen) :-
    arg(Idx, Files, file(FIdx, At, Len)),
    arg(Len, MinLen, MinIdx),
    find_blank(Blanks, At, Len, MinIdx, At1, MinIdx1),
    nb_setarg(Len, MinLen, MinIdx1),
    nb_setarg(Idx, Files, file(FIdx, At1, Len)),
    Idx1 is Idx + 1,
    compact_files(Files, Blanks, Idx1, MinLen).

find_blank(Blanks, At, _Len, Idx, At, Idx) :-
    \+ arg(Idx, Blanks, _), !.
find_blank(Blanks, At, Len, Idx, At1, Idx) :-
    arg(Idx, Blanks, At1-BLen),
    BLen >= Len, At1 < At,
    NewAtt is At1 + Len,
    NewLen is BLen - Len,
    nb_setarg(Idx, Blanks, NewAtt-NewLen), !.
find_blank(Blanks, At, Len, Idx, At1, Idx1) :-
    Next is Idx + 1,
    find_blank(Blanks, At, Len, Next, At1, Idx1).

score_file(file(FNum, At, Len), Score) :-
    End is At + Len - 1,
    aggregate_all(sum(N), between(At, End, N), Sum),
    Score is Sum * FNum.

% Parsing

from_file(List) :-
    phrase_from_file((digits(Raw), blank), "day09.txt"),
    maplist(plus(-48), Raw, List).

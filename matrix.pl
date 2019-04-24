:- consult('utils.pl').

% Matrix operations
row(Matrix, N, Row) :-
    nth1(N, Matrix, Row).

col(Matrix, N, Col) :-
    maplist(nth1(N), Matrix, Col).

index(Matrix, I, J, Elem) :-
    row(Matrix, I, Row), nth1(J, Row, Elem).

rows(Matrix, Length) :- length(Matrix, Length).

columns(Matrix, Length) :- row(Matrix, 1, Row), length(Row, Length).

replace([], _, _, []).
replace([_ | List], 1, Elem, [Elem | List]).
replace([X | List], Index, Elem, [X | List2]) :- NIndex is Index - 1, replace(List, NIndex, Elem, List2).

replace(Matrix, I, J, Elem, NewMatrix) :- 
    row(Matrix, I, OldRow), 
    replace(OldRow, J, Elem, NewRow),
    replace(Matrix, I, NewRow, NewMatrix).

validPos(Env, (R, C)) :- columns(Env, N), rows(Env, M), C > 0, C =< N, R > 0, R =< M.

directions8([(-1, 0), (-1, 1), (0, 1), (1, 1), (1, 0), (1, -1), (0, -1), (-1, -1)]).
directions4([(-1, 0), (0, 1), (1, 0), (0, -1)]).

neighborhood(_, _, _, [], []).
neighborhood(Env, R, C, [(X, Y) | Dirc], Neighbors) :-
    R1 is R + X, C1 is C + Y,
    % validPos(Env, R1, C1),
    append([(R1, C1)], NewNeighbors, AllNeighbors),
    neighborhood(Env, R, C, Dirc, NewNeighbors),
    include(validPos(Env), AllNeighbors, Neighbors).
    
indices(Env, Indices) :-
    rows(Env, N),
    columns(Env, M),
    range(1, N, RangeRows),
    range(1, M, RangeColumns),
    cartprod([RangeRows, RangeColumns], Indices).

% neighborhood(_, _, _, [], []).
% neighborhood(Env, R, C, [(X, Y) | Dirc], Neighbors) :-
%     R1 is R + X, C1 is C + Y,
%     validPos(Env, R1, C1),
%     index(Env, R1, C1, Tuple),
%     append([Tuple], NewNeighbors, Neighbors),
%     neighborhood(Env, R, C, Dirc, NewNeighbors).

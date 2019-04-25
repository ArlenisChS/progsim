:- consult('utils.pl').

% Tuple structure: (dirty, obstacle, yard, child, robot)

% Matrix operations
row(Matrix, N, Row) :-
    nth1(N, Matrix, Row).

col(Matrix, N, Col) :-
    maplist(nth1(N), Matrix, Col).

index(Matrix, I, J, Tuple) :-
    row(Matrix, I, Row), nth1(J, Row, Tuple).

rows(Matrix, Length) :- 
    length(Matrix, Length).

columns(Matrix, Length) :- 
    row(Matrix, 1, Row), length(Row, Length).

replace([_ | List], 1, Elem, [Elem | List]) :- !.
replace([X | List], Index, Elem, [X | List2]) :- 
    Index > 1, NIndex is Index - 1, 
    replace(List, NIndex, Elem, List2), !.

replace(Matrix, I, J, Elem, NewMatrix) :- 
    row(Matrix, I, OldRow), 
    replace(OldRow, J, Elem, NewRow),
    replace(Matrix, I, NewRow, NewMatrix).

validPos(Env, (R, C)) :- columns(Env, N), rows(Env, M), C > 0, C =< N, R > 0, R =< M.

directions8([(-1, 0), (-1, 1), (0, 1), (1, 1), (1, 0), (1, -1), (0, -1), (-1, -1)]).
directions4([(-1, 0), (0, 1), (1, 0), (0, -1)]).

neighborhood(_, _, _, [], []) :- !.
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

get_random_element([X], [], X).
get_random_element(List, Rest, Element) :- 
    length(List, L), random_between(1, L, C), 
    nth1(C, List, Element, Rest).

count_object(_, [], _, 0) :- !.
count_object(Env, [(X, Y) | List], Mask, Count) :-
    index(Env, X, Y, Elem),
    bitwise_and(Elem, Mask, (X1, X2, X3, X4, X5)),
    C1 is X1 + X2 + X3 + X4 + X5,
    count_object(Env, List, Mask, C2),
    Count is C1 + C2.

my_random8(A, B) :- 
    random_between(1, 8, C), directions8(Dirc), 
    nth1(C, Dirc, (A, B)).

my_random4(A, B) :- 
    random_between(1, 4, C), directions4(Dirc),
    nth1(C, Dirc, (A, B)).
    
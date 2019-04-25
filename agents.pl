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

is_clean((0, _, _, _, _)).

is_row_clean([]).
is_row_clean([H | T]) :- 
    is_clean(H), is_row_clean(T).

is_env_clean([]).
is_env_clean([Row | Env]) :- 
    is_row_clean(Row), is_env_clean(Env).

count_clean_row([], 0).
count_clean_row([(0, _, _, _, _) | Row], Count) :- 
    count_clean_row(Row, C), !, Count is C + 1.
count_clean_row([(1, _, _, _, _) | Row], Count) :- 
    count_clean_row(Row, Count), !.

count_clean([], 0).
count_clean([Row | Env], Count) :- 
    count_clean_row(Row, C1), !, 
    count_clean(Env, C2), !, 
    Count is C1 + C2.

count_dirty_row([], 0).
count_dirty_row([(1, _, _, _, _) | Row], Count) :- 
    count_dirty_row(Row, C), !, Count is C + 1.
count_dirty_row([(0, _, _, _, _) | Row], Count) :- 
    count_dirty_row(Row, Count), !.

count_dirty([], 0).
count_dirty([Row | Env], Count) :- 
    count_dirty_row(Row, C1), !, 
    count_dirty(Env, C2), !, 
    Count is C1 + C2.

count_empty_row([], 0).
count_empty_row([(_, 0, 0, _, _) | Row], Count) :- 
    count_empty_row(Row, C), !, Count is C + 1.
count_empty_row([(_, X2, X3, _, _) | Row], Count) :- 
    (X2 == 1; X3 == 1), !, 
    count_empty_row(Row, Count), !.

count_empty([], 0).
count_empty([Row | Env], Count) :- 
    count_empty_row(Row, C1), !, 
    count_empty(Env, C2), !, 
    Count is C1 + C2.

children_captured_row([]).
children_captured_row([(_, _, 1, 1, _)|Row]) :- 
    children_captured_row(Row), !.
children_captured_row([(_, _, _, 0, _)|Row]) :- 
    children_captured_row(Row), !.

children_captured([]).
children_captured([Row | Env]) :- 
    children_captured_row(Row), 
    children_captured(Env).

% There's more than 60% of the env dirty
polluted(X) :- 
    count_empty(X, C), count_dirty(X, R), 
    R > ((C/100)*60).

final_state([]).
final_state(X) :- 
    is_env_clean(X), !, children_captured(X).
final_state(X) :- 
    polluted(X).

% inRange
validPos(Env, R, C) :- 
    columns(Env, N), rows(Env, M), 
    C > 0, C =< N, R > 0, R =< M.

% Tuple structure: (dirty, obstacle, yard, child, robot)
move_objects(Env1, R1, C1, A, B, Env2) :- 
    C2 is C1+B, R2 is R1+A, validPos(Env1, R2, C2), 
    index(Env1, R1, C1, (X11, _, X13, X14, X15)), 
    index(Env1, R2, C2, (0, 0, 0, 0, 0)), !, 
    replace(Env1, R1, C1, (X11, 0, X13, X14, X15), Env3), 
    replace(Env3, R2, C2, (0, 1, 0, 0, 0), Env2).
move_objects(Env1, R1, C1, A, B, Env2) :- 
    C2 is C1+B, R2 is R1+A, validPos(Env1, R2, C2), 
    index(Env1, R1, C1, (X11, _, X13, X14, X15)), 
    index(Env1, R2, C2, (X21, 1, X23, X24, X25)), 
    move_objects(Env1, R2, C2, A, B, Env3), 
    replace(Env3, R1, C1, (X11, 0, X13, X14, X15), Env4), 
    replace(Env4, R2, C2, (X21, 1, X23, X24, X25), Env2).


% Tuple structure: (dirty, obstacle, yard, child, robot)
move_child(Env1, R1, C1, A, B, Env2) :- 
    C2 is C1+B, R2 is R1+A, validPos(Env1, R2, C2), 
    index(Env1, R1, C1, (X11, X12, X13, _, X15)), 
    index(Env1, R2, C2, (X21, 0, 0, 0, 0)), !,
    replace(Env1, R1, C1, (X11, X12, X13, 0, X15), Env3), 
    replace(Env3, R2, C2, (X21, 0, 0, 1, 0), Env2).
move_child(Env1, R1, C1, A, B, Env2) :- 
    C2 is C1+B, R2 is R1+A, validPos(Env1, R2, C2), !, 
    index(Env1, R1, C1, (X11, X12, X13, _, X15)), 
    index(Env1, R2, C2, (X21, 1, X23, _, X25)),
    move_objects(Env1, R2, C2, A, B, Env3), !,
    replace(Env3, R1, C1, (X11, X12, X13, 0, X15), Env4), 
    replace(Env4, R2, C2, (X21, 0, X23, 1, X25), Env2).
move_child(Env1, _, _, _, _, Env1).

directions8([(-1, 0), (-1, 1), (0, 1), (1, 1), (1, 0), (1, -1), (0, -1), (-1, -1)]).
directions4([(-1, 0), (0, 1), (1, 0), (0, -1)]).

child_neighborhood(_, _, _, [], 0).
child_neighborhood(Env, R, C, [(A, B) | Dirc], Count):-
    child_neighborhood(Env, R, C, Dirc, Count2), 
    R1 is R+A, C1 is C+B, 
    index(Env, R1, C1, (_, _, _, Count1, _)), 
    Count is Count1 + Count2. 

my_random8(A, B) :- 
    random_between(1, 8, C), directions8(Dirc), 
    nth1(C, Dirc, (A, B)).

my_random4(A, B) :- 
    random_between(1, 4, C), directions4(Dirc),
    nth1(C, Dirc, (A, B)).

get_random_element([X], [], X).
get_random_element(List, Rest, Element) :- 
    length(List, L), random_between(1, L, C), 
    nth1(C, List, Element, Rest).

mess_direc(Env1, R, C, Env2) :-
    validPos(Env1, R, C), 
    index(Env1, R, C, (0, 0, 0, X4, X5)), !,
    replace(Env1, R, C, (1, 0, 0, X4, X5), Env2).
mess_direc(Env1, _, _, Env1).

mess_child_count(Env1, R, C, Env2, Count) :- 
    Count == 0, !, directions8(Direc), 
    get_random_element(Direc, _, (X, Y)), 
    R1 is R+X, C1 is C+Y, 
    mess_direc(Env1, R1, C1, Env2).
mess_child_count(Env1, R, C, Env2, Count) :- 
    Count == 1, !, directions8(Direc), 
    get_random_element(Direc, Rest1, (X1, Y1)), 
    R1 is R+X1, C1 is C+Y1, 
    mess_direc(Env1, R1, C1, Env3),
    get_random_element(Rest1, _, (X2, Y2)), 
    R2 is R+X2, C2 is C+Y2, 
    mess_direc(Env3, R2, C2, Env2).
mess_child_count(Env1, R, C, Env2, _) :- 
    directions8(Direc), 
    get_random_element(Direc, Rest1, (X1, Y1)), 
    R1 is R+X1, C1 is C+Y1, 
    mess_direc(Env1, R1, C1, Env3),
    get_random_element(Rest1, Rest2, (X2, Y2)), 
    R2 is R+X2, C2 is C+Y2, 
    mess_direc(Env3, R2, C2, Env4),
    get_random_element(Rest2, Rest3, (X3, Y3)), 
    R3 is R+X3, C3 is C+Y3, 
    mess_direc(Env4, R3, C3, Env5),
    get_random_element(Rest3, Rest4, (X4, Y4)), 
    R4 is R+X4, C4 is C+Y4, 
    mess_direc(Env5, R4, C4, Env6),
    get_random_element(Rest4, Rest5, (X5, Y5)), 
    R5 is R+X5, C5 is C+Y5, 
    mess_direc(Env6, R5, C5, Env7),
    get_random_element(Rest5, _, (X6, Y6)), 
    R6 is R+X6, C6 is C+Y6, 
    mess_direc(Env7, R6, C6, Env2).

mess_child(Env1, R, C, Env2) :- 
    directions8(Direc), 
    child_neighborhood(Env1, R, C, Direc, Count),
    mess_child_count(Env1, R, C, Env2, Count).

child(Env1, R, C, Env2) :- 
    mess_child(Env1, R, C, Env3), 
    my_random8(A, B), 
    move_child(Env3, R, C, A, B, Env2).

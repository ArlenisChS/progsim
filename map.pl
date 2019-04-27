:- consult('matrix.pl').

is_clean((0, _, _, _, _)).
is_dirty((1, _, _, _, _)).

is_row_clean([]).
is_row_clean([H | T]) :- 
    is_clean(H), is_row_clean(T).

% Tuple structure: (dirty, obstacle, yard, child, robot)
% Is the environment clean, i.e. 0% dirty

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

final_state(X) :- 
    is_env_clean(X), !, children_captured(X), assertz(win(1)).
final_state(X) :- 
    polluted(X), 
    assertz(lose(1)), 
    count_dirty(X, Count),
    rows(X, R), columns(X, C),
    Size is R * C,
    Percent is (100 * Count) / Size,
    assertz(polluted_cells(Percent)).

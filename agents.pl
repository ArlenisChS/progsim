:- consult('matrix.pl').

% Tuple structure: (dirty, obstacle, yard, child, robot)
% Is the environment clean, i.e. 0% dirty
is_dirty((1, _, _, _, _)).

is_env_clean([]).
is_env_clean([Row | Env]) :- is_row_clean(Row), is_env_clean(Env).
% is_env_clean([X | Y]) :- not(is_dirty(X, _)), is_env_clean(Y).
% is_env_clean([(X1, _, _, _, _) | Y]) :- X1 == 0, is_env_clean(Y).

is_row_clean([]).
is_row_clean([H | T]) :- not(is_dirty(H)), is_row_clean(T).

count_clean([], 0).
count_clean([Row | Env], Count) :- not(is_dirty(Row)), count_clean(Env, C), Count is C + 1.
% count_clean([(X1, _, _, _, _)|Y], Z) :- X1 == 0, count_clean(Y, C), Z is C+1.
% count_clean([(X1, _, _, _, _)|Y], Z) :- X1 == 1, count_clean(Y, C), Z is C.

count_dirty([], 0).
count_dirty([X | Y], Z) :- is_dirty(X), count_clean(Y, C), Z is C + 1.
% count_dirty([(X1, _, _, _, _)|Y], Z) :- X1 == 1, count_dirty(Y, C), Z is C+1.
% count_dirty([(X1, _, _, _, _)|Y], Z) :- X1 == 0, count_dirty(Y, C), Z is C.

% Tengo dudas aqui, `child` y `robot` no cuentan como "llenar" la casilla?
count_empty([], 0).
count_empty([(_, 0, 0, _, _)|Y], Z) :- count_empty(Y, C), Z is C+1.
count_empty([(_, 1, _, _, _)|Y], C) :- count_empty(Y, C).
count_empty([(_, _, 1, _, _)|Y], C) :- count_empty(Y, C).

% No entiendo los cortes T_T
children_captured([]).
children_captured([(_, _, 1, 1, _)|Y]) :- children_captured(Y).
children_captured([(_, _, _, 0, _)|Y]) :- children_captured(Y).

% There's more than 60% of the env dirty
poluted(X) :- count_empty(X, C), count_dirty(X, R), R > ((C/100)*60).

final_state([]).
final_state(X) :- is_env_clean(X), !, children_captured(X).
final_state(X) :- poluted(X).

% % Given a row and a column, return index
% indexFrom(Env, Row, Column, Index) :- 
%     length(Env, Length), 
%     Index is (Length / Row) * (Row - 1) + Column.

% % Better?
% get(Env, Row, Column, Elem) :- 
%     indexFrom(Env, Row, Column, Index),
%     nth1(Index, Env, Elem).

% get_tupla([X|_], _, 1, 1, X).
% get_tupla([_|Y], N, C, R, T) :- C==1, C2 is N, R2 is R-1, get_tupla(Y, N, C2, R2, T).
% get_tupla([_|Y], N, C, R, T) :- C=\=1, C2 is C-1, get_tupla(Y, N, C2, R, T).

% replace(X, [_|Y], _, [X|Y], 1, 1).
% replace(X, [Z|Y], N, [Z|P], C, R) :- C==1, C2 is N, R2 is R-1, replace(X, Y, N, P, C2, R2).
% replace(X, [Z|Y], N, [Z|P], C, R) :- C=\=1, C2 is C-1, replace(X, Y, N, P, C2, R).


% Tuple structure: (dirty, obstacle, yard, child, robot)
move_objects(Env1, R1, C1, A, B, Env2) :- 
    C2 is C1+B, R2 is R1+A, validPos(Env1, (R2, C2)), 
    index(Env1, R1, C1, (X11, _, X13, X14, X15)), 
    index(Env1, R2, C2, (0, 0, 0, 0, 0)), !, 
    replace(Env1, R1, C1, (X11, 0, X13, X14, X15), Env3), 
    replace(Env3, R2, C2, (0, 1, 0, 0, 0), Env2).
move_objects(Env1, R1, C1, A, B, Env2) :- 
    C2 is C1+B, R2 is R1+A, validPos(Env1, (R2, C2)), 
    index(Env1, R1, C1, (X11, _, X13, X14, X15)), 
    index(Env1, R2, C2, (X21, 1, X23, X24, X25)), 
    move_objects(Env1, R2, C2, A, B, Env3), 
    replace(Env3, R1, C1, (X11, 0, X13, X14, X15), Env4), 
    replace(Env4, R2, C2, (X21, 1, X23, X24, X25), Env2).


% Tuple structure: (dirty, obstacle, yard, child, robot)
move_child(Env1, R1, C1, A, B, Env2) :- 
    C2 is C1+B, R2 is R1+A, validPos(Env1, (R2, C2)), 
    index(Env1, R1, C1, (X11, X12, X13, _, X15)), 
    index(Env1, R2, C2, (X21, 0, 0, 0, 0)), !,
    replace(Env1, R1, C1, (X11, X12, X13, 0, X15), Env3), 
    replace(Env3, R2, C2, (X21, 0, 0, 1, 0), Env2).
move_child(Env1, R1, C1, A, B, Env2) :- 
    C2 is C1+B, R2 is R1+A, validPos(Env1, (R2, C2)), !, 
    index(Env1, R1, C1, (X11, X12, X13, _, X15)), 
    index(Env1, R2, C2, (X21, 1, X23, _, X25)),
    move_objects(Env1, R2, C2, A, B, Env3), !,
    replace(Env3, R1, C1, (X11, X12, X13, 0, X15), Env4), 
    replace(Env4, R2, C2, (X21, 0, X23, 1, X25), Env2).
move_child(Env1, _, _, _, _, Env1).

% child_neighborhood(_, _, _, [], 0).
% child_neighborhood(Env, R, C, [(X, Y) | Dirc], Count):-
%     child_neighborhood(Env, R, C, Dirc, OldCount), 
%     R1 is R + X, C1 is C + Y, index(Env, R1, C1, (_, _, _, IsChild, _)), 
%     Count is IsChild + OldCount. 

% Given a list of positions, count kids.
neighboring_child_count(_, [], 0).
neighboring_child_count(Env, [(I, J) | T], Count) :-
    index(Env, I, J, (_, _, _, IsChild, _)), 
    neighboring_child_count(Env, T, NewCount), Count is NewCount + IsChild.
% neighboring_child_count([(_, _, _, IsChild, _) | Neighbors], Count) :- 
%     neighboring_child_count(Neighbors, NewCount), Count is NewCount + IsChild.

my_random8(A, B) :- 
    random_between(1, 8, C), directions8(Dirc), nth1(C, Dirc, (A, B)).

my_random4(A, B) :- 
    random_between(1, 4, C), directions4(Dirc), nth1(C, Dirc, (A, B)).

give_random_pos([X], [], X).
eliminar_elemento_random(P1, P2, X) :- length_Lista(P1, L), random_between(1, L, C), eliminar_elemento(P1, P2, C, X).

eliminar_elemento([X|Y], [Y], 1, X).
eliminar_elemento([X|Y], [X|Z], Count, C) :- Count2 is Count-1, eliminar_elemento(Y, Z, Count2, C).

agregar_tupla_a_lista(P1, N, M, C, R, P1) :- not(validPos(N, (M, C), R)), !.
agregar_tupla_a_lista(P1, N, M, C, R, [(C, R)|P1]) :- validPos(N, (M, C), R).

formar_lista_8v(N, M, C, R, L1) :- C1 is C+1, C2 is C-1, R1 is R+1, R2 is R-1, agregar_tupla_a_lista([], N, M, C, R2, L2), agregar_tupla_a_lista(L2, N, M, C1, R2, L3), agregar_tupla_a_lista(L3, N, M, C1, R, L4), agregar_tupla_a_lista(L4, N, M, C1, R1, L5), agregar_tupla_a_lista(L5, N, M, C, R1, L6), agregar_tupla_a_lista(L6, N, M, C2, R1, L7), agregar_tupla_a_lista(L7, N, M, C2, R, L8), agregar_tupla_a_lista(L8, N, M, C2, R2, L1).

ensuciar_si_es_posible(P1, N, P1, C, R) :- get_tupla(P1, N, C, R, (X1, X2, X3, _, _)), (X1 == 1; X2 == 1; X3 == 1), !.
ensuciar_si_es_posible(P1, N, P2, C, R) :- get_tupla(P1, N, C, R, (X1, X2, X3, X4, X5)), X1 == 0, X2 == 0, X3 == 0, replace((1, X2, X3, X4, X5), P1, N, P2, C, R).

ensuciar_segun_ninnos(P1, N, P2, Count, L) :- Count == 0, !, eliminar_elemento_random(L, _, (C1, R1)), ensuciar_si_es_posible(P1, N, P2, C1, R1).
ensuciar_segun_ninnos(P1, N, P2, Count, L) :- Count == 1, !, eliminar_elemento_random(L, _, (C1, R1)), ensuciar_si_es_posible(P1, N, P2, C1, R1).

ninno_ensuciar(P1, N, M, C, R, P2) :- contar_ninnos_en_vecindad(P1, N, M, C, R, Count), formar_lista_8v(N, M, C, R, L1), ensuciar_segun_ninnos(P1, N, P2, Count, L1).

agente_ninno(P1, N, M, C, R, P2) :- my_random_8(A, B), ninno_ensuciar(P1, N, M, C, R, P3), move_child(P3, N, M, C, R, P2, A, B).

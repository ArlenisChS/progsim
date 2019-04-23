% Tuple structure: (dirty, obstacle, yard, child, robot)

% Is the environment clean, i.e. 0% dirty
is_dirty((0, _, _, _, _), 0).
is_dirty((1, _, _, _, _), 1).

is_env_clean([]).
is_env_clean([X | Y]) :- not(is_dirty(X, _)), is_env_clean(Y).
% is_env_clean([(X1, _, _, _, _) | Y]) :- X1 == 0, is_env_clean(Y).

count_clean([], 0).
count_clean([X | Y], Z) :- not(is_dirty(X, T)), count_clean(Y, C), Z is C + T.
% count_clean([(X1, _, _, _, _)|Y], Z) :- X1 == 0, count_clean(Y, C), Z is C+1.
% count_clean([(X1, _, _, _, _)|Y], Z) :- X1 == 1, count_clean(Y, C), Z is C.

count_dirty([], 0).
count_dirty([X | Y], Z) :- is_dirty(X, T), count_clean(Y, C), Z is C + T.
% count_dirty([(X1, _, _, _, _)|Y], Z) :- X1 == 1, count_dirty(Y, C), Z is C+1.
% count_dirty([(X1, _, _, _, _)|Y], Z) :- X1 == 0, count_dirty(Y, C), Z is C.

% Tengo dudas aqui, `child` y `robot` no cuentan como "llenar" la casilla?
count_empty([], 0).
count_empty([(_, X2, X3, _, _)|Y], Z) :- X2 == 0, X3 == 0, count_empty(Y, C), Z is C+1.
count_empty([(_, X2, X3, _, _)|Y], Z) :- (X2 == 1; X3 == 1), count_empty(Y, C), Z is C.

% No entiendo los cortes T_T
children_captured([]).
children_captured([(_, _, X3, X4, _)|Y]) :- X4 == 1, !, X3 == 1, !, children_captured(Y).
children_captured([(_, _, _, X4, _)|Y]) :- X4 == 0, children_captured(Y).

% There's more than 60% of the env dirty
poluted(X) :- count_empty(X, C), count_dirty(X, R), R > ((C/100)*60).

final_state([]).
final_state(X) :- is_env_clean(X), !, children_captured(X).
final_state(X) :- poluted(X).

% Es esto inRange?
se_mantiene_en_terreno(N, M, C, R) :- C > 0, C =< N, R > 0, R =< M.

% Better?
get(Env, Row, Column, Elem) :- 
    length(Env, Length), 
    Index is (Length / Row) * (Row - 1) + Column,
    nth1(Index, Env, Elem).

get_tupla([X|_], _, 1, 1, X).
get_tupla([_|Y], N, C, R, T) :- C==1, C2 is N, R2 is R-1, get_tupla(Y, N, C2, R2, T).
get_tupla([_|Y], N, C, R, T) :- C=\=1, C2 is C-1, get_tupla(Y, N, C2, R, T).

sustituir_tupla(X, [_|Y], _, [X|Y], 1, 1).
sustituir_tupla(X, [Z|Y], N, [Z|P], C, R) :- C==1, C2 is N, R2 is R-1, sustituir_tupla(X, Y, N, P, C2, R2).
sustituir_tupla(X, [Z|Y], N, [Z|P], C, R) :- C=\=1, C2 is C-1, sustituir_tupla(X, Y, N, P, C2, R).

mover_objetos(P1, N, M, C1, R1, A, B, P2) :- C2 is C1+A, R2 is R1+B, se_mantiene_en_terreno(N, M, C2, R2), get_tupla(P1, N, C1, R1, (X11, _, X13, X14, X15)), get_tupla(P1, N, C2, R2, (X21, X22, X23, X24, X25)), X21==0, X22==0, X23==0, X24==0, X25==0, sustituir_tupla((X11, 0, X13, X14, X15), P1, N, P3, C1, R1), sustituir_tupla((X21, 1, X23, X24, X25), P3, N, P2, C2, R2).
mover_objetos(P1, N, M, C1, R1, A, B, P2) :- C2 is C1+A, R2 is R1+B, se_mantiene_en_terreno(N, M, C2, R2), get_tupla(P1, N, C1, R1, (X11, _, X13, X14, X15)), get_tupla(P1, N, C2, R2, (X21, X22, X23, X24, X25)), X22==1, mover_objetos(P1, N, M, C2, R2, A, B, P3), sustituir_tupla((X11, 0, X13, X14, X15), P3, N, P4, C1, R1), sustituir_tupla((X21, 1, X23, X24, X25), P4, N, P2, C2, R2).

mover_ninno(P1, N, M, C1, R1, P2, A, B) :- C2 is C1+A, R2 is R1+B, se_mantiene_en_terreno(N, M, C2, R2), get_tupla(P1, N, C1, R1, (X11, X12, X13, _, X15)), get_tupla(P1, N, C2, R2, (X21, X22, X23, X24, X25)), X22==0, X23==0, X24==0, X25==0, sustituir_tupla((X21, X22, X23, 1, X25), P1, N, P3, C1, R1), sustituir_tupla((X11, X12, X13, 0, X15), P3, N, P2, C2, R2).
mover_ninno(P1, N, M, C1, R1, P2, A, B) :- C2 is C1+A, R2 is R1+B, se_mantiene_en_terreno(N, M, C2, R2), get_tupla(P1, N, C1, R1, (X11, X12, X13, _, X15)), get_tupla(P1, N, C2, R2, (X21, X22, X23, _, X25)), X22==1, mover_objetos(P1, N, M, C2, R2, A, B, P3), sustituir_tupla((X11, X12, X13, 0, X15), P3, N, P4, C1, R1), sustituir_tupla((X21, X22, X23, 1, X25), P4, N, P2, C2, R2).
mover_ninno(P1, _, _, _, _, P1, _, _).

contar_ninno_1(P1, N, M, C, R, A) :- R1 is R-1, se_mantiene_en_terreno(N, M, C, R1), get_tupla(P1, N, C, R1, (_, _, _, A, _)).
contar_ninno_2(P1, N, M, C, R, A) :- R1 is R-1, C1 is C+1, se_mantiene_en_terreno(N, M, C1, R1), get_tupla(P1, N, C1, R1, (_, _, _, A, _)).
contar_ninno_3(P1, N, M, C, R, A) :- C1 is C+1, se_mantiene_en_terreno(N, M, C1, R), get_tupla(P1, N, C1, R, (_, _, _, A, _)).
contar_ninno_4(P1, N, M, C, R, A) :- R1 is R+1, C1 is C+1, se_mantiene_en_terreno(N, M, C1, R1), get_tupla(P1, N, C1, R1, (_, _, _, A, _)).
contar_ninno_5(P1, N, M, C, R, A) :- R1 is R+1, se_mantiene_en_terreno(N, M, C, R1), get_tupla(P1, N, C, R1, (_, _, _, A, _)).
contar_ninno_6(P1, N, M, C, R, A) :- R1 is R+1, C1 is C-1, se_mantiene_en_terreno(N, M, C1, R1), get_tupla(P1, N, C1, R1, (_, _, _, A, _)).
contar_ninno_7(P1, N, M, C, R, A) :- C1 is C-1, se_mantiene_en_terreno(N, M, C1, R), get_tupla(P1, N, C1, R, (_, _, _, A, _)).
contar_ninno_8(P1, N, M, C, R, A) :- R1 is R-1, C1 is C-1, se_mantiene_en_terreno(N, M, C1, R1), get_tupla(P1, N, C1, R1, (_, _, _, A, _)).

contar_ninnos_en_vecindad(P1, N, M, C, R, Count) :- contar_ninno_1(P1, N, M, C, R, A1),contar_ninno_2(P1, N, M, C, R, A2), contar_ninno_3(P1, N, M, C, R, A3), contar_ninno_4(P1, N, M, C, R, A4), contar_ninno_5(P1, N, M, C, R, A5), contar_ninno_6(P1, N, M, C, R, A6), contar_ninno_7(P1, N, M, C, R, A7), contar_ninno_8(P1, N, M, C, R, A8), Count is A1+A2+A3+A4+A5+A6+A7+A8.

my_random_8(A, B) :- random_between(1, 8, C), posicion_8(A, B, C).

my_random_4(A, B) :- random_between(1, 4, C), posicion_4(A, B, C).

posicion_8(A, B, C) :- C == 1, A is 0, B is -1.
posicion_8(A, B, C) :- C == 2, A is 1, B is -1.
posicion_8(A, B, C) :- C == 3, A is 1, B is 0.
posicion_8(A, B, C) :- C == 4, A is 1, B is 1.
posicion_8(A, B, C) :- C == 5, A is 0, B is 1.
posicion_8(A, B, C) :- C == 6, A is -1, B is 1.
posicion_8(A, B, C) :- C == 7, A is -1, B is 0.
posicion_8(A, B, C) :- C == 8, A is -1, B is -1.

posicion_4(A, B, C) :- C == 1, A is 0, B is -1.
posicion_4(A, B, C) :- C == 2, A is 1, B is 0.
posicion_4(A, B, C) :- C == 3, A is 0, B is 1.
posicion_4(A, B, C) :- C == 4, A is -1, B is 0.

length_Lista([], 0).
length_Lista([_|Y], C) :- length_Lista(Y, C1), C is C1+1.

eliminar_elemento_random([X], [], X).
eliminar_elemento_random(P1, P2, X) :- length_Lista(P1, L), random_between(1, L, C), eliminar_elemento(P1, P2, C, X).

eliminar_elemento([X|Y], [Y], 1, X).
eliminar_elemento([X|Y], [X|Z], Count, C) :- Count2 is Count-1, eliminar_elemento(Y, Z, Count2, C).

agregar_tupla_a_lista(P1, N, M, C, R, P1) :- not(se_mantiene_en_terreno(N, M, C, R)), !.
agregar_tupla_a_lista(P1, N, M, C, R, [(C, R)|P1]) :- se_mantiene_en_terreno(N, M, C, R).

formar_lista_8v(N, M, C, R, L1) :- C1 is C+1, C2 is C-1, R1 is R+1, R2 is R-1, agregar_tupla_a_lista([], N, M, C, R2, L2), agregar_tupla_a_lista(L2, N, M, C1, R2, L3), agregar_tupla_a_lista(L3, N, M, C1, R, L4), agregar_tupla_a_lista(L4, N, M, C1, R1, L5), agregar_tupla_a_lista(L5, N, M, C, R1, L6), agregar_tupla_a_lista(L6, N, M, C2, R1, L7), agregar_tupla_a_lista(L7, N, M, C2, R, L8), agregar_tupla_a_lista(L8, N, M, C2, R2, L1).

ensuciar_si_es_posible(P1, N, P1, C, R) :- get_tupla(P1, N, C, R, (X1, X2, X3, _, _)), (X1 == 1; X2 == 1; X3 == 1), !.
ensuciar_si_es_posible(P1, N, P2, C, R) :- get_tupla(P1, N, C, R, (X1, X2, X3, X4, X5)), X1 == 0, X2 == 0, X3 == 0, sustituir_tupla((1, X2, X3, X4, X5), P1, N, P2, C, R).

ensuciar_segun_ninnos(P1, N, P2, Count, L) :- Count == 0, !, eliminar_elemento_random(L, _, (C1, R1)), ensuciar_si_es_posible(P1, N, P2, C1, R1).
ensuciar_segun_ninnos(P1, N, P2, Count, L) :- Count == 1, !, eliminar_elemento_random(L, _, (C1, R1)), ensuciar_si_es_posible(P1, N, P2, C1, R1).

ninno_ensuciar(P1, N, M, C, R, P2) :- contar_ninnos_en_vecindad(P1, N, M, C, R, Count), formar_lista_8v(N, M, C, R, L1), ensuciar_segun_ninnos(P1, N, P2, Count, L1).

agente_ninno(P1, N, M, C, R, P2) :- my_random_8(A, B), ninno_ensuciar(P1, N, M, C, R, P3), mover_ninno(P3, N, M, C, R, P2, A, B).

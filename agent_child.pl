% Tuple structure: (dirty, obstacle, yard, child, robot)
:- consult('matrix.pl').
:- consult('map.pl').

% Tuple structure: (dirty, obstacle, yard, child, robot)
move_objects(Env1, R1, C1, A, B, Env2) :- 
    C2 is C1+B, R2 is R1+A, validPos(Env1, (R2, C2)), 
    index(Env1, R1, C1, (X11, _, X13, X14, X15)), 
    index(Env1, R2, C2, (0, 0, 0, 0, 0)), !, 
    replace(Env1, R1, C1, (X11, 0, X13, X14, X15), Env3), 
    replace(Env3, R2, C2, (0, 1, 0, 0, 0), Env2),
    retract(obstacle(_,(R1, C1))), 
    assertz(obstacle(_,(R2, C2))).
move_objects(Env1, R1, C1, A, B, Env2) :- 
    C2 is C1+B, R2 is R1+A, validPos(Env1, (R2, C2)), 
    index(Env1, R1, C1, (X11, _, X13, X14, X15)), 
    index(Env1, R2, C2, (X21, 1, X23, X24, X25)), 
    move_objects(Env1, R2, C2, A, B, Env3), !,
    replace(Env3, R1, C1, (X11, 0, X13, X14, X15), Env4), 
    replace(Env4, R2, C2, (X21, 1, X23, X24, X25), Env2),
    retract(obstacle(_,(R1, C1))), 
    assertz(obstacle(_,(R2, C2))).

% Tuple structure: (dirty, obstacle, yard, child, robot)
move_child(Env1, R1, C1, A, B, Env2) :- 
    C2 is C1+B, R2 is R1+A, validPos(Env1, (R2, C2)), 
    % writeln("1"),
    index(Env1, R1, C1, (X11, X12, X13, _, X15)),
    % writeln("2"), 
    index(Env1, R2, C2, (X21, 0, 0, 0, 0)), !,
    % writeln("3"),
    replace(Env1, R1, C1, (X11, X12, X13, 0, X15), Env3),
    % writeln("4"), 
    replace(Env3, R2, C2, (X21, 0, 0, 1, 0), Env2),
    % writeln("5"),
    assertz(child(_, (R2, C2))), !.
move_child(Env1, R1, C1, A, B, Env2) :- 
    C2 is C1+B, R2 is R1+A, validPos(Env1, (R2, C2)),
    % writeln("6"),
    index(Env1, R1, C1, (X11, X12, X13, _, X15)),
    % writeln("7"), 
    index(Env1, R2, C2, (X21, 1, X23, _, X25)),
    % writeln("8"),
    move_objects(Env1, R2, C2, A, B, Env3), !,
    % writeln("9"),
    replace(Env3, R1, C1, (X11, X12, X13, 0, X15), Env4), 
    % writeln("10"),
    replace(Env4, R2, C2, (X21, 0, X23, 1, X25), Env2),
    % writeln("11"),
    assertz(child(_, (R2, C2))), !.
move_child(Env1, _, _, _, _, Env1).

% Given a list of positions, count kids.
neighboring_child_count(_, [], 0) :- !.
neighboring_child_count(Env, [(I, J) | T], Count) :-
    index(Env, I, J, (_, _, _, IsChild, _)), 
    neighboring_child_count(Env, T, NewCount), Count is NewCount + IsChild.
% neighboring_child_count([(_, _, _, IsChild, _) | Neighbors], Count) :- 
%     neighboring_child_count(Neighbors, NewCount), Count is NewCount + IsChild.

mess_direc(Env1, R, C, Env2) :-
    validPos(Env1, (R, C)), 
    % writeln((R, C)),
    index(Env1, R, C, (0, 0, 0, X4, X5)), !,
    % writeln("cumple"),
    replace(Env1, R, C, (1, 0, 0, X4, X5), Env2).
    % writeln("cumple2").
mess_direc(Env1, _, _, Env1) :- !.

mess_child_count(Env1, R, C, Env2, Count) :- 
    Count == 0, !, directions8(Direc), 
    get_random_element(Direc, _, (X, Y)), 
    R1 is R+X, C1 is C+Y, 
    mess_direc(Env1, R1, C1, Env2).
mess_child_count(Env1, R, C, Env3, Count) :- 
    Count == 1, !, directions8(Direc), 
    get_random_element(Direc, Rest1, (X1, Y1)), 
    R1 is R+X1, C1 is C+Y1, 
    mess_direc(Env1, R1, C1, Env2),
    get_random_element(Rest1, _, (X2, Y2)), 
    R2 is R+X2, C2 is C+Y2, 
    mess_direc(Env2, R2, C2, Env3).
mess_child_count(Env1, R, C, Env7, _) :- 
    directions8(Direc), 
    get_random_element(Direc, Rest1, (X1, Y1)), 
    R1 is R+X1, C1 is C+Y1, 
    mess_direc(Env1, R1, C1, Env2),
    get_random_element(Rest1, Rest2, (X2, Y2)), 
    R2 is R+X2, C2 is C+Y2, 
    mess_direc(Env2, R2, C2, Env3),
    get_random_element(Rest2, Rest3, (X3, Y3)), 
    R3 is R+X3, C3 is C+Y3, 
    mess_direc(Env3, R3, C3, Env4),
    get_random_element(Rest3, Rest4, (X4, Y4)), 
    R4 is R+X4, C4 is C+Y4, 
    mess_direc(Env4, R4, C4, Env5),
    get_random_element(Rest4, Rest5, (X5, Y5)), 
    R5 is R+X5, C5 is C+Y5, 
    mess_direc(Env5, R5, C5, Env6),
    get_random_element(Rest5, _, (X6, Y6)), 
    R6 is R+X6, C6 is C+Y6, 
    mess_direc(Env6, R6, C6, Env7).

mess_child(Env1, R, C, Env2) :- 
    directions8(Direc),
    neighborhood(Env1, R, C, Direc, Neigh), 
    % open("output.txt", append, Stream),    
    % writeln(Stream, ("Direction", R, C)),
    % writeln(Stream, "neighborhood"),
    % writeln(Stream, Neigh),   
    % close(Stream),
    neighboring_child_count(Env1, Neigh, Count),
    % open("output.txt", append, Stream1),    
    % writeln(Stream1, Count), 
    % close(Stream1),
    mess_child_count(Env1, R, C, Env2, Count).

child(Env1, R, C, Env2) :- 
    mess_child(Env1, R, C, Env3), 
    % open("output.txt", append, Stream),    
    % writeln(Stream, "ya ensucio"),
    my_random8(A, B), 
    % writeln(Stream, (A, B)),    
    % writeln(Stream, "ya escoge direc random"),
    move_child(Env3, R, C, A, B, Env2).
    % writeln(Stream, "ya termino este"),
    % close(Stream). 

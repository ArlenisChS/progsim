% Tuple structure: (dirty, obstacle, yard, child, robot)
:- consult('matrix.pl').
:- consult('map.pl').
:- consult('bfs.pl').
:- dynamic(caughtChild/1, path/1).

% No se si hace falta comprobar validPos
move_robot_with_child(Env1, I1, J1, I2, J2, Env2):-
    validPos(Env1, (I2, J2)),
    index(Env1, I2, J2, (X21, 0, X23, 0, 0)),
    index(Env1, I1, J1, (X11, 0, X13, 1, 1)), !,
    replace(Env1, I1, J1, (X11, 0, X13, 0, 0), Env3),
    replace(Env3, I2, J2, (X21, 0, X23, 1, 1), Env2),
    retract(robot(_,_)), assert(robot(_, (I2, J2))).
% move_robot_with_child(Env1, _, _, _, _, Env1):-writeln("aki2").

move_robot_without_child(Env1, I1, J1, I2, J2, Env2):-
    validPos(Env1, (I2, J2)),  
    index(Env1, I2, J2, (X21, 0, X23, X24, 0)),
    index(Env1, I1, J1, (X11, 0, X13, X14, 1)), !,
    replace(Env1, I1, J1, (X11, 0, X13, X14, 0), Env3),
    replace(Env3, I2, J2, (X21, 0, X23, X24, 1), Env2),
    retract(robot(_,_)), assert(robot(_, (I2, J2))).
% move_robot_without_child(Env1, _, _, _, _, Env1).

robot_clean_if_dirty(Env1, I, J, Env2):-
    % writeln("aaaaa"),
    index(Env1, I, J, (1, 0, 0, 0, X5)), !,
    replace(Env1, I, J, (0, 0, 0, 0, X5), Env2), !.
robot_clean_if_dirty(Env1, _, _, Env1).

% bfs_shortest_path_dirt(Env1, I, J, Path).
% bfs_shortest_path_kid(Env1, I, J, Path).
% bfs_shortest_path_yard(Env1, I, J, Path).
% path(P).
% caughtChild(B).

robot1(Env1, X, Y, Env1):-
    caughtChild(Bool), Bool =:= 1,
    % writeln("caso0part1"),
    index(Env1, X, Y, (_, _, 1, 1, _)), !,
    % writeln("caso0part2"),
    retract(caughtChild(_)),
    assertz(caughtChild(0)).
robot1(Env1, _, _, Env1):-
    caughtChild(Bool), Bool =:= 1,
    findall(P1, path(P1), _),
    % writeln(Path),
    % writeln("caso1part1"),
    findall(P, path(P), [(X0, Y0)|_]), X0 =:= 0, Y0 =:= 0, !,
    % writeln("caso1part2"),
    retract(path(_)). 
robot1(Env1, I, J, Env2):-
    caughtChild(Bool), Bool =:= 1,
    % writeln("caso3part1"),
    findall(P, path(P), [(X1, Y1), (X0, Y0) |_]),
    % writeln([(X1, Y1), (X0, Y0)|_]),
    X0 =:= 0, Y0 =:= 0,
    move_robot_with_child(Env1, I, J, X1, Y1, Env2), !,
    retractall(path(_)).
    % writeln("caso3part2").
robot1(Env1, I, J, Env2):-
    caughtChild(Bool), Bool =:= 1,
    % writeln("caso4part1"),
    findall(P1, path(P1), [(_, _)|_]),
    retract(path(_)),
    % move_robot_with_child(Env1, I, J, X1, Y1, Env3),
    findall(P2, path(P2), [(X2, Y2)|_]),
    retract(path(_)),
    % writeln([(X2, Y2)|_]),
    % writeln("caso4part2"),
    move_robot_with_child(Env1, I, J, X2, Y2, Env2), !.
robot1(Env1, I, J, Env2):-
    caughtChild(Bool), Bool =:= 0,
    % writeln("caso6part1"),
    % writeln(I), writeln(J),   
    index(Env1, I, J, (1, _, _, 0, _)), !,
    % writeln("caso6part2"),
    robot_clean_if_dirty(Env1, I, J, Env2).
robot1(Env1, I, J, Env1):-
    caughtChild(Bool), Bool =:= 0,
    % writeln("caso7part1"),
    index(Env1, I, J, (_, _, 1, 1, _)),
    % writeln("caso7part2"),
    bfs_shortest_path_dirt_kid(Env1, [(I, J)], 100000),
    findall(P, path(P), [(X0, Y0)|_]), retractall(path(_)), X0 =:= 0, Y0 =:= 0, !.  
robot1(Env1, I, J, Env2):-
    caughtChild(Bool), Bool =:= 0,
    % writeln("caso8part1"),
    index(Env1, I, J, (_, _, 1, 1, _)),
    % writeln("caso8part2"),
    bfs_shortest_path_dirt_kid(Env1, [(I, J)], 100000),
    findall(P1, path(P1), [(X1, Y1)|_]), retractall(path(_)),
    move_robot_without_child(Env1, I, J, X1, Y1, Env2),
    index(Env2, X1, Y1, (_, _, 0, 1, _)), !,
    retract(caughtChild(_)), assertz(caughtChild(1)),
    bfs_shortest_path_yard(Env2, [(X1, Y1)], 100000).
robot1(Env1, I, J, Env2):-
    caughtChild(Bool), Bool =:= 0,
    % writeln("caso9part1"),
    index(Env1, I, J, (_, _, 1, 1, _)),
    % writeln("caso9part2"),
    bfs_shortest_path_dirt_kid(Env1, [(I, J)], 100000),
    findall(P, path(P), [(X1, Y1)|_]), retractall(path(_)),
    move_robot_without_child(Env1, I, J, X1, Y1, Env2),
    index(Env2, X1, Y1, (_, _, _, 0, _)), !.
robot1(Env1, I, J, Env1):-
    caughtChild(Bool), Bool =:= 0,
    % writeln("caso10part1"),
    bfs_shortest_path_dirt_kid(Env1, [(I, J)], 100000),
    findall(P, path(P), [(X0, Y0)|_]), 
    % writeln((X0, Y0)), writeln(Path), 
    retractall(path(_)),
    X0 =:= 0, Y0 =:= 0, !.
    % writeln("caso10part2"). 
robot1(Env1, I, J, Env2):-
    caughtChild(Bool), Bool =:= 0,
    % writeln("caso11part1"),
    % writeln(I), writeln(J),
    % findall(P, path(P), Path), 
    % writeln(Path),
    % findall(PQ, parent(_, PQ), PathQ), 
    % writeln(PathQ),
    % writeln("caso11part2"),
    % listing(robot),
    % retractall(visited(_, _)),
    bfs_shortest_path_dirt_kid(Env1, [(I, J)], 100000),
    % listing(path),
    % listing(visited),
    % writeln("caso11part3"),
    % findall(P1, path(P1), Path1), writeln(Path1),
    findall(P, path(P), [(X1, Y1)|_]), 
    % writeln("caso11part3.5"),
    % writeln((X1, Y1)), writeln(Path), 
    retractall(path(_)),
    % writeln(X1), writeln(Y1),
    % writeln("caso11part4"),
    move_robot_without_child(Env1, I, J, X1, Y1, Env2),
    % writeln("caso11part5"),
    index(Env2, X1, Y1, (_, _, 0, 1, _)), !,
    % writeln("caso11part6"),
    retractall(caughtChild(_)), assertz(caughtChild(1)),
    bfs_shortest_path_yard(Env2, [(X1, Y1)], 100000).
    % findall(P1, path(P1), Path212),
    % writeln(Path212).
robot1(Env1, I, J, Env2):-
    caughtChild(Bool), Bool =:= 0,
    % writeln("caso12part1"),
    % writeln(I), writeln(J),
    % findall(P, path(P), Path), 
    % writeln(Path),
    % findall(PQ, parent(_, PQ), PathQ), 
    % writeln(PathQ),
    % writeln("caso12part2"),
    % listing(parent),
    % retractall(visited(_, _)),
    bfs_shortest_path_dirt_kid(Env1, [(I, J)], 100000),
    % writeln("caso12part3"),
    % findall(P1, path(P1), Path1), writeln(Path1),
    findall(P, path(P), [(X1, Y1)|_]), 
    % writeln((X1, Y1)), writeln(Path), 
    retractall(path(_)),
    % writeln(X1), writeln(Y1),
    % writeln("caso12part4"),
    move_robot_without_child(Env1, I, J, X1, Y1, Env2), !.
robot1(Env1, _, _, Env1).

% robot2(Env1, I, J, Env1):-
%     caughtChild(Bool), Bool =:= 1,
%     writeln("caso1part1"),
%     index(Env1, I, J, (_, _, 1, 0, _)), !,
%     retract(caughtChild(_)),
%     assertz(caughtChild(0)).
robot2(Env1, I, J, Env1):-
    caughtChild(Bool), Bool =:= 1,
    % writeln("caso2part1"),
    bfs_shortest_path_yard(Env1, [(I, J)], 100000),
    findall(P, path(P), [(X0, Y0)|_]),
    % writeln([(X0, Y0)|_]), 
    retractall(path(_)),
    X0 =:= 0, Y0 =:= 0, !.
robot2(Env1, I, J, Env2):-
    caughtChild(Bool), Bool =:= 1,
    % writeln("caso3part1"),
    bfs_shortest_path_yard(Env1, [(I, J)], 100000),
    findall(P1, path(P1), [(X, Y), (X0, Y0)|_]),
    % writeln([(X, Y), (X0, Y0)|_]),
    retractall(path(_)), X0 =:= 0, Y0 =:= 0,
    move_robot_with_child(Env1, I, J, X, Y, Env2),
    index(Env1, X, Y, (_, _, 1, 0, _)), !,
    retract(caughtChild(_)),
    assertz(caughtChild(0)).
robot2(Env1, I, J, Env2):-
    caughtChild(Bool), Bool =:= 1,
    % writeln("caso3.2part1"),
    bfs_shortest_path_yard(Env1, [(I, J)], 100000),
    findall(P1, path(P1), [(X, Y), (X0, Y0)|_]),
    retractall(path(_)), 
    % writeln([(X, Y), (X0, Y0)|_]),
    X0 =:= 0, Y0 =:= 0,
    move_robot_with_child(Env1, I, J, X, Y, Env2), !.
robot2(Env1, I, J, Env2):-
    caughtChild(Bool), Bool =:= 1,
    % writeln("caso4part1"),
    bfs_shortest_path_yard(Env1, [(I, J)], 100000),
    findall(P1, path(P1), [(_, _), (X2, Y2)|_]),
    retractall(path(_)), 
    % writeln([(X, Y), (X2, Y2)|Path]),
    move_robot_with_child(Env1, I, J, X2, Y2, Env2),
    index(Env1, X2, Y2, (_, _, 1, 0, _)), !,
    retract(caughtChild(_)),
    assertz(caughtChild(0)).
robot2(Env1, I, J, Env2):-
    caughtChild(Bool), Bool =:= 1,
    % writeln("caso4.2part1"),
    bfs_shortest_path_yard(Env1, [(I, J)], 100000),
    findall(P1, path(P1), [(_, _), (X2, Y2)|_]),
    retractall(path(_)),
    % writeln([(X, Y), (X2, Y2)|Path]),
    move_robot_with_child(Env1, I, J, X2, Y2, Env2), !.
robot2(Env1, I, J, Env2):-
    caughtChild(Bool), Bool =:= 0,
    % writeln("caso5part1"),
    index(Env1, I, J, (1, _, _, _, _)), !,
    robot_clean_if_dirty(Env1, I, J, Env2).
robot2(Env1, I, J, Env1):-
    caughtChild(Bool), Bool =:= 0,
    % writeln("caso6part1"),
    index(Env1, I, J, (_, _, 1, 1, _)),
    bfs_shortest_path_kid(Env1, [(I, J)], 100000),
    findall(P1, path(P1), [(X1, Y1)|_]), 
    % writeln([(X1, Y1)|_]),
    retractall(path(_)), X1 =:= 0, Y1 =:= 0,
    bfs_shortest_path_dirt(Env1, [(I, J)], 100000),
    findall(P2, path(P2), [(X0, Y0)|_]), 
    % writeln([(X0, Y0)|_]),
    retractall(path(_)), X0 =:= 0, Y0 =:= 0, !.  
robot2(Env1, I, J, Env2):-
    caughtChild(Bool), Bool =:= 0,
    % writeln("caso7part1"),
    index(Env1, I, J, (_, _, 1, 1, _)),
    bfs_shortest_path_kid(Env1, [(I, J)], 100000),
    findall(P1, path(P1), [(X0, Y0)|_]), 
    % writeln([(X0, Y0)|_]),
    retractall(path(_)), X0 =:= 0, Y0 =:= 0,
    bfs_shortest_path_dirt(Env1, [(I, J)], 100000),
    findall(P, path(P), [(X, Y)|_]), retractall(path(_)),
    % writeln([(X, Y)|_]),
    move_robot_without_child(Env1, I, J, X, Y, Env2), !.
robot2(Env1, I, J, Env2):-
    caughtChild(Bool), Bool =:= 0,
    % writeln("caso8part1"),
    index(Env1, I, J, (_, _, 1, 1, _)),
    bfs_shortest_path_kid(Env1, [(I, J)], 100000),
    findall(P, path(P), [(X, Y)|_]), 
    % writeln([(X, Y)|_]), 
    retractall(path(_)),
    move_robot_without_child(Env1, I, J, X, Y, Env2),
    index(Env2, X, Y, (_, _, 0, 1, _)), !,
    retract(caughtChild(_)), assertz(caughtChild(1)).
robot2(Env1, I, J, Env2):-
    caughtChild(Bool), Bool =:= 0,
    % writeln("caso9part1"),
    index(Env1, I, J, (_, _, 1, 1, _)),
    bfs_shortest_path_kid(Env1, [(I, J)], 100000),
    findall(P, path(P), [(X, Y)|_]), 
    % writeln([(X, Y)|_]), 
    retractall(path(_)),
    move_robot_without_child(Env1, I, J, X, Y, Env2), !.
% robot2(Env1, I, J, Env1):-
%     caughtChild(Bool), Bool =:= 0,
    % writeln("caso9part1"),
%     index(Env1, I, J, (_, _, 0, 1, _)),
%     retract(caughtChild(_)), assertz(caughtChild(1)),
%     bfs_shortest_path_yard(Env1, [(I, J)], 100000),
%     retract(caughtChild(_)), assertz(caughtChild(0)),
%     findall(P, path(P), [(X0, Y0)|_]), 
%     writeln([(X0, Y0)|_]),
%     retractall(path(_)), X0 =:= 0, Y0 =:= 0,
%     retract(caughtChild(_)), assertz(caughtChild(1)), !.
% robot2(Env1, I, J, Env2):-
%     caughtChild(Bool), Bool =:= 0,
    % writeln("caso10part1"),
%     index(Env1, I, J, (_, _, 0, 1, _)), !,
%     retract(caughtChild(_)), assertz(caughtChild(1)),
%     bfs_shortest_path_yard(Env1, [(I, J)], 100000),
%     findall(P, path(P), [(X, Y)|_]), retractall(path(_)),
%     writeln([(X, Y)|_]),
%     move_robot_with_child(Env1, I, J, X, Y, Env2).
robot2(Env1, I, J, Env1):-
    caughtChild(Bool), Bool =:= 0,
    % writeln("caso11part1"),
    index(Env1, I, J, (_, _, _, 0, _)),
    % writeln("caso11part2"),
    bfs_shortest_path_kid(Env1, [(I, J)], 100000),
    % writeln("caso11part3"),
    findall(P1, path(P1), [(X0, Y0)|_]), 
    % writeln([(X0, Y0)|_]),
    retractall(path(_)), X0 =:= 0, Y0 =:= 0,
    bfs_shortest_path_dirt(Env1, [(I, J)], 100000),
    findall(P2, path(P2), [(X1, Y1)|_]), 
    % writeln([(X1, Y1)|_]), 
    retractall(path(_)), X1 =:= 0, Y1 =:= 0, ! .
robot2(Env1, I, J, Env2):-
    caughtChild(Bool), Bool =:= 0,
    % writeln("caso12part1"),
    index(Env1, I, J, (_, _, _, 0, _)),
    bfs_shortest_path_kid(Env1, [(I, J)], 100000),
    findall(P1, path(P1), [(X0, Y0)|_]),
    %  writeln([(X0, Y0)|_]),
    retractall(path(_)), X0 =:= 0, Y0 =:= 0,
    bfs_shortest_path_dirt(Env1, [(I, J)], 100000),
    findall(P2, path(P2), [(X, Y)|_]), retractall(path(_)),
    move_robot_without_child(Env1, I, J, X, Y, Env2), !,
    retractall(path(_)).
robot2(Env1, I, J, Env2):-
    caughtChild(Bool), Bool =:= 0,
    % writeln("caso13part1"),
    index(Env1, I, J, (_, _, _, 0, _)),
    bfs_shortest_path_kid(Env1, [(I, J)], 100000),
    findall(P, path(P), [(X, Y)|_]),
    % writeln([(X, Y)|_]),
    retractall(path(_)),
    move_robot_without_child(Env1, I, J, X, Y, Env2),
    index(Env2, X, Y, (_, _, 0, 1, _)), !,
    retract(caughtChild(_)), assertz(caughtChild(1)).
robot2(Env1, I, J, Env2):-
    caughtChild(Bool), Bool =:= 0,
    % writeln("caso14part1"),
    index(Env1, I, J, (_, _, _, 0, _)),
    bfs_shortest_path_kid(Env1, [(I, J)], 100000),
    findall(P, path(P), [(X, Y)|_]), 
    % writeln([(X, Y)|_]),
    retractall(path(_)), 
    move_robot_without_child(Env1, I, J, X, Y, Env2), !.
robot2(Env1, _, _, Env1).

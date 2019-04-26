% Tuple structure: (dirty, obstacle, yard, child, robot)
:- consult('matrix.pl').
:- consult('map.pl').

% No se si hace falta comprobar validPos
move_robot_with_child(Env1, I1, J1, I2, J2, Env2):-
    validPos(Env1, (I2, J2)),
    index(Env1, I2, J2, (X21, 0, X23, 0, 0)), !,
    index(Env1, I1, J1, (X11, 0, X13, 1, 1)), !,
    replace(Env1, I1, J1, (X11, 0, X13, 0, 0), Env3),
    replace(Env3, I2, J2, (X21, 0, X23, 1, 1), Env2).
move_robot_with_child(Env1, _, _, _, _, Env1).

move_robot_without_child(Env1, I1, J1, I2, J2, Env2):-
    validPos(Env1, (I2, J2)),  
    index(Env1, I2, J2, (X21, 0, X23, X24, 0)), !,
    print(X24),
    index(Env1, I1, J1, (X11, 0, X13, X14, 1)), !,
    replace(Env1, I1, J1, (X11, 0, X13, X14, 0), Env3),
    replace(Env3, I2, J2, (X21, 0, X23, X24, 1), Env2).
move_robot_without_child(Env1, _, _, _, _, Env1).

robot_clean_if_dirty(Env1, I, J, Env2):-
    print("aaaaa"),
    index(Env1, I, J, (1, 0, 0, X4, X5)), !,
    replace(Env1, I, J, (0, 0, 0, X4, X5), Env2).
robot_clean_if_dirty(Env1, _, _, Env1).

% BFS_closest_dirt(Env1, I, J, Path).
% BFS_closest_child(Env1, I, J, Path).
% BFS_closest_corral(Env1, I, J, Path).
% path(P).
% caughtChild(B).
bfs_closest_child_dirt(_).

agent_robot1(Env1, I, J, Env1):-
    caughtChild(Bool), Bool == 1,
    path((0, 0)), !,
    retract(path(_)), retract(caughtChild(_)),
    assert(caughtChild(0)).
agent_robot1(Env1, I, J, Env1):-
    caughtChild(Bool), Bool == 1,
    path((X, Y)),
    move_robot_with_child(Env1, I, J, X, Y, Env1), !.  
agent_robot1(Env1, I, J, Env2):-
    caughtChild(Bool), Bool == 1,
    path((X1, Y1)),
    move_robot_with_child(Env1, I, J, X1, Y1, Env2),
    retract(path(_)),
    path((0, 0)), !,
    retract(path(_)), retract(caughtChild(_)),
    assert(caughtChild(0)).
agent_robot1(Env1, I, J, Env2):-
    caughtChild(Bool), Bool == 1,
    path((X1, Y1)),
    move_robot_with_child(Env1, I, J, X1, Y1, Env2),
    retract(path(_)),
    path((X2, Y2)),
    move_robot_with_child(Env2, I, J, X, Y, Env2), !.
agent_robot1(Env1, I, J, Env2):-
    caughtChild(Bool), Bool == 1,
    path((X1, Y1)),
    move_robot_with_child(Env1, I, J, X1, Y1, Env3),
    retract(path(_)),
    path((X2, Y2)),
    move_robot_with_child(Env3, I, J, X, Y, Env2), !,
    retract(path(_)).
agent_robot1(Env1, I, J, Env2):-
    caughtChild(Bool), Bool == 0,
    index(Env1, I, J, (1, _, _, _, _)), !,
    robot_clean_if_dirty(Env1, I, J, Env2).
agent_robot1(Env1, I, J, Env1):-
    caughtChild(Bool), Bool == 0,
    index(Env1, I, J, (_, _, 1, 1, _)), !,
    assert(parent((0, 0), (I, J))),
    higher_order_bfs(Env1, [bfs_closest_child_dirt, [(I, J)]], 100000),
    path((0, 0)), retract(path(_)).  
agent_robot1(Env1, I, J, Env2):-
    caughtChild(Bool), Bool == 0,
    index(Env1, I, J, (_, _, 1, 1, _)), !,
    assert(parent((0, 0), (I, J))),
    higher_order_bfs(Env1, [bfs_closest_child_dirt, [(I, J)]], 100000),
    path((X1, Y1)), 
    move_robot_without_child(Env1, I, J, X1, Y1, Env2),
    retractall(path(_)).
agent_robot1(Env1, I, J, Env1):-
    caughtChild(Bool), Bool == 0,
    index(Env1, I, J, (_, _, 0, 1, _)), !,
    retract(caughtChild(_)),
    assert(caughtChild(1)),
    assert(parent((0, 0), (I, J))),
    higher_order_bfs(Env1, [bfs_closest_child_dirt, [(I, J)]], 100000),
    path((X1, Y1)).
agent_robot1(Env1, I, J, Env2):-
    caughtChild(Bool), Bool == 0,
    index(Env1, I, J, (_, _, 0, 1, _)), !,
    retract(caughtChild(_)),
    assert(caughtChild(1)),
    assert(parent((0, 0), (I, J))),
    higher_order_bfs(Env1, [bfs_closest_child_dirt, [(I, J)]], 100000).


agent_robot2(Env1, I, J, Env1):-
    caughtChild(Bool), Bool == 1,
    path((0, 0)), !,
    retract(path(_)), retract(caughtChild(_)),
    assert(caughtChild(0)).
agent_robot2(Env1, I, J, Env1):-
    caughtChild(Bool), Bool == 1,
    path((X, Y)),
    move_robot_with_child(Env1, I, J, X, Y, Env1), !.  
agent_robot2(Env1, I, J, Env2):-
    caughtChild(Bool), Bool == 1,
    path((X1, Y1)),
    move_robot_with_child(Env1, I, J, X1, Y1, Env2),
    retract(path(_)),
    path((0, 0)), !,
    retract(path(_)), retract(caughtChild(_)),
    assert(caughtChild(0)).
agent_robot2(Env1, I, J, Env2):-
    caughtChild(Bool), Bool == 1,
    path((X1, Y1)),
    move_robot_with_child(Env1, I, J, X1, Y1, Env2),
    retract(path(_)),
    path((X2, Y2)),
    move_robot_with_child(Env2, I, J, X, Y, Env2), !.
agent_robot2(Env1, I, J, Env2):-
    caughtChild(Bool), Bool == 1,
    path((X1, Y1)),
    move_robot_with_child(Env1, I, J, X1, Y1, Env3),
    retract(path(_)),
    path((X2, Y2)),
    move_robot_with_child(Env3, I, J, X, Y, Env2), !,
    retract(path(_)).
agent_robot2(Env1, I, J, Env2):-
    caughtChild(Bool), Bool == 0,
    index(Env1, I, J, (1, _, _, _, _)), !,
    robot_clean_if_dirty(Env1, I, J, Env2).
agent_robot2(Env1, I, J, Env1):-
    caughtChild(Bool), Bool == 0,
    index(Env1, I, J, (_, _, 1, 1, _)), !,
    assert(parent((0, 0), (I, J))),
    higher_order_bfs(Env1, [bfs_closest_child_dirt, [(I, J)]], 100000),
    path((0, 0)), retract(path(_)).  
agent_robot2(Env1, I, J, Env2):-
    caughtChild(Bool), Bool == 0,
    index(Env1, I, J, (_, _, 1, 1, _)), !,
    assert(parent((0, 0), (I, J))),
    higher_order_bfs(Env1, [bfs_closest_child_dirt, [(I, J)]], 100000),
    path((X1, Y1)), 
    move_robot_without_child(Env1, I, J, X1, Y1, Env2),
    retractall(path(_)).
agent_robot2(Env1, I, J, Env1):-
    caughtChild(Bool), Bool == 0,
    index(Env1, I, J, (_, _, 0, 1, _)), !,
    retract(caughtChild(_)),
    assert(caughtChild(1)),
    assert(parent((0, 0), (I, J))),
    higher_order_bfs(Env1, [bfs_closest_child_dirt, [(I, J)]], 100000),
    path((X1, Y1)).
agent_robot2(Env1, I, J, Env2):-
    caughtChild(Bool), Bool == 0,
    index(Env1, I, J, (_, _, 0, 1, _)), !,
    retract(caughtChild(_)),
    assert(caughtChild(1)),
    assert(parent((0, 0), (I, J))),
    higher_order_bfs(Env1, [bfs_closest_child_dirt, [(I, J)]], 100000).
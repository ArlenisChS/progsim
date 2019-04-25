:- consult('matrix.pl').
:- consult('bfs.pl').
:- dynamic(yard/2, dirt/2, child/2, robot/2, children/2, obstacles/2, mess/2, playpen/2).


% Tuple structure: (dirty, obstacle, yard, child, robot)

% Generate an environment
% n - Row count
% m - Columns count
% dirty - Percent of dirty
% obstacles - Obstacle percent
% children - Children count
generate(N, M, DirtyPercent, ObstaclePercent, ChildCount, Environment) :-
    empty(N, M, Env),
    Xn is (N div 2) + 1, Xm is (M div 2) + 1, Center = (Xn, Xm),
    % Place playpen
    higher_order_bfs(Env, [generate_yard, [Center]], ChildCount),
    findall(Yard, yard(_, Yard), PlayPen),
    writeln("PlayPen"),
    writeln(PlayPen),
    place_items(Env, PlayPen, (0, 0, 1, 0, 0), EnvWithPlayPen),
    writeln("EnvWithPlayPen"),
    writeln(EnvWithPlayPen),
    % Place obstacles
    Size is N * M, ObstacleCount is round(Size * ObstaclePercent),
    generate_obstacles(EnvWithPlayPen, ObstacleCount),
    findall(Obstacle, obstacle(_, Obstacle), Obstacles),
    writeln("Obstacles"),
    writeln(Obstacles),
    place_items(EnvWithPlayPen, Obstacles, (0, 1, 0, 0, 0), EnvWithObstacles),
    writeln("EnvWithObstacles"),
    writeln(EnvWithObstacles),
    % Place dirt
    DirtCount is round(Size * DirtyPercent),
    writeln("Mess1"),
    generate_mess(EnvWithObstacles, DirtCount),
    writeln("Mess2"),
    findall(Dirt, dirt(_, Dirt), Mess),
    writeln("Mess3"),
    writeln(Mess),
    place_items(EnvWithObstacles, Mess, (1, 0, 0, 0, 0), EnvWithMess),
    writeln("EnvWithMess"),
    writeln(EnvWithMess),
    % Place kids
    generate_children(EnvWithMess, ChildCount),
    findall(Child, child(_, Child), Children),
    writeln("Children"),
    writeln(Children),
    place_items(EnvWithMess, Children, (0, 0, 0, 1, 0), EnvWithChildren),
    writeln("EnvWithChildren"),
    writeln(EnvWithChildren),
    % Place robot
    generate_robot(EnvWithChildren),
    robot(_, Robot),
    place_items(EnvWithChildren, Robot, (0, 0, 0, 0, 1), Environment).
    
% Bfs that iteratively generates yards
% Env   => The map
% Queue => Current state of the queue
% NewQueue => Next state of the queue
% generate_yard(Env, Queue, NewQueue)
generate_yard(_, [], _) :- retractall(visited(_, _)), !.
generate_yard(Env, [(I, J) | Queue], NewQueue) :-
    expand(Env, [(I, J) | Queue], NewQueue),
    assertz(visited(Env, (I, J))),
    assertz(yard(Env, (I, J))).

generate_obstacle(Env) :-
    writeln("Generate Obstacle"),
    findall(X, yard(Env, X), PlayPen),
    indices(Env, Indices),
    writeln(Indices),
    subtract(Indices, PlayPen, Available),
    writeln(Available),
    random_member(Elem, Available),
    writeln("Available to place obstacles"),
    assertz(obstacle(Env, Elem)), !.

generate_obstacles(_, 0) :- !.
generate_obstacles(Env, Amount) :-
    Amount > 0,
    writeln("Generate Obstacles recursion case"),
    generate_obstacle(Env),
    NewAmount is Amount - 1,
    generate_obstacles(Env, NewAmount).

generate_dirt(Env) :-
    % findall(X, yard(Env, X), PlayPen),
    % findall(Y, obstacle(Env, Y), Obstacles),
    writeln("aaa1"),
    findall(X, yard(_, X), PlayPen),
    writeln("aaa2"),
    % listing(obstacles),
    findall(Obstacle, obstacle(_, Obstacle), Obstacles), 
    writeln(PlayPen),
    writeln(Obstacles),
    indices(Env, Indices),
    writeln(Indices),
    subtract(Indices, PlayPen, NoPlayPen),
    writeln(NoPlayPen),
    subtract(NoPlayPen, Obstacles, Available),
    writeln(Available),
    random_member(Elem, Available),
    writeln("Available to place dirt"),
    assertz(dirt(Env, Elem)).

generate_mess(_, 0) :- !.
generate_mess(Env, Amount) :-
    Amount > 0,
    generate_dirt(Env),
    NewAmount is Amount - 1,
    generate_mess(Env, NewAmount).

generate_child(Env) :-
    % findall(X, yard(Env, X), PlayPen),
    % findall(Y, obstacle(Env, Y), Obstacles),
    findall(Yard, yard(_, Yard), PlayPen),
    findall(Obstacle, obstacle(_, Obstacle), Obstacles), 
    indices(Env, Indices),
    subtract(Indices, PlayPen, NoPlayPen),
    subtract(NoPlayPen, Obstacles, Available),
    random_member(Elem, Available),
    writeln("Available to place children"),
    writeln(Available),
    assertz(child(Env, Elem)).

generate_children(_, 0) :- !.
generate_children(Env, Amount) :-
    Amount > 0,
    generate_child(Env),
    NewAmount is Amount - 1,
    generate_children(Env, NewAmount).

generate_robot(Env) :-
    % findall(X, yard(Env, X), PlayPen),
    % findall(Y, obstacle(Env, Y), Obstacles),
    % findall(Y, child(Env, Y), Children),
    findall(Yard, yard(_, Yard), PlayPen),
    findall(Obstacle, obstacle(_, Obstacle), Obstacles), 
    findall(Child, child(_, Child), Children), 
    % children(_, Children),
    indices(Env, Indices),
    subtract(Indices, PlayPen, NoPlayPen),
    subtract(NoPlayPen, Children, NoChildren),
    subtract(NoChildren, Obstacles, Available),
    random_member(Elem, Available),
    writeln("Available to place robot"),
    writeln(Available),
    assertz(robot(Env, [Elem])).

place_items(Env, [], _, Env) :- 
    writeln("Inside Place base case"),
    writeln(Env).
% place_items(Env, [], _, Env) :- !.
place_items(Env, [(I, J) | Indices], Mask, FinalEnv) :-
    writeln("Inside Place recursion case"),
    index(Env, I, J, Tuple),
    bitwise_or(Tuple, Mask, NewElem),
    replace(Env, I, J, NewElem, NewEnv),
    place_items(NewEnv, Indices, Mask, FinalEnv).


% The empty position.
empty((0, 0, 0, 0, 0)).

% The empty array of size `M`.
empty(0, []) :- !.
empty(M, L) :-
    empty(Tuple),
    append([Tuple], X, L),
    M1 is M - 1,
    empty(M1, X).

% The empty initial map with `N` rows and `M` columns
empty(0, _, []) :- !.
empty(N, M, Env) :-
    empty(M, Row),
    append([Row], NewEnv, Env),
    N1 is N - 1,
    empty(N1, M, NewEnv).

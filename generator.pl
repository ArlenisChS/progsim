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
    clean(),
    empty(N, M, Env),
    random_between(1, N, RN), random_between(1, M, RM),
    % Xn is (N div 2) + 1, Xm is (M div 2) + 1, Center = (Xn, Xm),
    % Place playpen
    % higher_order_bfs(Env, [generate_yard, [(RN, RM)]], ChildCount),
    bfs_generate_yard(Env, [(RN, RM)], ChildCount),
    % writeln("Generated Yard"),
    findall(Yard, yard(_, Yard), PlayPen),
    % playpen(_, PlayPen), 
    place_items(Env, PlayPen, (0, 0, 1, 0, 0), EnvWithPlayPen),
    % Place obstacles
    Size is N * M, ObstacleCount is round(Size * ObstaclePercent),
    generate_obstacles(EnvWithPlayPen, ObstacleCount),
    findall(O, obstacle(_, O), Obstacles),
    % obstacles(EnvWithPlayPen, Obstacles), 
    place_items(EnvWithPlayPen, Obstacles, (0, 1, 0, 0, 0), EnvWithObstacles),
    % Place dirt
    DirtCount is round(Size * DirtyPercent),
    generate_mess(EnvWithObstacles, DirtCount),
    findall(M, dirt(_, M), Mess),
    % mess(EnvWithObstacles, Mess),
    place_items(EnvWithObstacles, Mess, (1, 0, 0, 0, 0), EnvWithMess),
    % Place kids
    generate_children(EnvWithMess, ChildCount),
    findall(C, child(_, C), Children),
    % children(EnvWithMess, Children),
    place_items(EnvWithMess, Children, (0, 0, 0, 1, 0), EnvWithChildren),
    % Place robot
    generate_robot(EnvWithChildren),
    robot(_, Robot),
    place_items(EnvWithChildren, Robot, (0, 0, 0, 0, 1), Environment),
    nl, printWorld(Environment), nl.
    
clean() :- 
    retractall(yard(_, _)),
    retractall(playpen(_, _)),
    retractall(child(_, _)),
    retractall(children(_, _)),
    retractall(obstacle(_, _)),
    retractall(obstacles(_, _)),
    retractall(dirt(_, _)),
    retractall(mess(_, _)),
    retractall(robot(_, _)).

% Bfs that iteratively generates yards
% Env   => The map
% Queue => Current state of the queue
% NewQueue => Next state of the queue
% generate_yard(_, Queue, NewQueue)
generate_yard(_, [], _) :- !.
generate_yard(Env, [(I, J) | Queue], NewQueue) :-
    expand(Env, [(I, J) | Queue], NewQueue),
    assertz(visited(_, (I, J))),
    assertz(yard(_, (I, J))).

generate_obstacle(Env) :-
    % findall(X, yard(_, X), Yards),
    findall(X, yard(_, X), Yards),
    indices(Env, Indices),
    findall(Z, obstacle(_, Z), Obstacles), 
    subtract(Indices, Yards, NoYards),
    subtract(NoYards, Obstacles, Available),
    random_member(Elem, Available),
    assertz(obstacle(_, Elem)), !.

generate_obstacles(_, 0) :- !.
generate_obstacles(Env, Amount) :-
    Amount > 0,
    generate_obstacle(Env),
    NewAmount is Amount - 1,
    generate_obstacles(Env, NewAmount).

generate_dirt(Env) :-
    % findall(X, yard(_, X), Yards),
    findall(Y, obstacle(_, Y), Obstacles),
    findall(X, yard(_, X), Yards),
    % obstacles(_, Obstacles),
    findall(Z, dirt(_, Z), Mess), 
    indices(Env, Indices),
    subtract(Indices, Yards, NoYards),
    subtract(NoYards, Mess, NoMess),
    subtract(NoMess, Obstacles, Available),
    random_member(Elem, Available),
    assertz(dirt(_, Elem)).

generate_mess(_, 0) :- !.
generate_mess(Env, Amount) :-
    Amount > 0,
    generate_dirt(Env),
    NewAmount is Amount - 1,
    generate_mess(Env, NewAmount).

generate_child(Env) :-
    findall(X, yard(_, X), Yards),
    findall(Y, obstacle(_, Y), Obstacles),
    % playpen(_, Yards),
    % obstacles(_, Obstacles),
    findall(Child, child(_, Child), Children),
    indices(Env, Indices),
    subtract(Indices, Yards, NoYards),
    subtract(NoYards, Children, NoChildren),
    subtract(NoChildren, Obstacles, Available),
    random_member(Elem, Available),
    assertz(child(_, Elem)).

generate_children(_, 0) :-  !.
generate_children(Env, Amount) :-
    Amount > 0,
    generate_child(Env),
    NewAmount is Amount - 1,
    generate_children(Env, NewAmount).

generate_robot(Env) :-
    findall(X, yard(_, X), Yards),
    findall(Y, obstacle(_, Y), Obstacles),
    findall(Z, child(_, Z), Children),
    % playpen(_, Yards),
    % obstacles(_, Obstacles),
    % children(_, Children),
    indices(Env, Indices),
    subtract(Indices, Yards, NoYards),
    subtract(NoYards, Children, NoChildren),
    subtract(NoChildren, Obstacles, Available),
    random_member(Elem, Available),
    assertz(robot(_, [Elem])).

place_items(Env, [], _, Env).
% place_items(Env, [], _, Env) :- !.
place_items(Env, [(I, J) | Indices], Mask, FinalEnv) :-
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

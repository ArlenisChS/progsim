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
generate(N, M, Dirty, Obstacles, Children) :-
    empty(N, M, Env),
    Xn is N / 2, Xm is M / 2, Center = (Xn, Xm),
    Size is N * M, ChildCount is Size * Obstacles,
    higher_order_bfs(Env, [generate_yard, [Center]], ChildCount),
    playpen(Env, Playpen), place_items(Env, Indices, (0, 0, 0, 1, 0), NewEnv).
    
% The empty position.
empty((0, 0, 0, 0, 0)).

% The empty array of size `M`.
empty(0, []).
empty(M, L) :-
    empty(Tuple),
    append([Tuple], X, L),
    M1 is M - 1,
    empty(M1, X).

% The empty initial map with `N` rows and `M` columns
empty(0, _, []).
empty(N, M, Env) :-
    empty(M, Row),
    append([Row], NewEnv, Env),
    N1 is N - 1,
    empty(N1, M, NewEnv).

% Bfs that iteratively generates yards
% Env   => The map
% Queue => Current state of the queue
% NewQueue => Next state of the queue
% generate_yard(Env, Queue, NewQueue)
generate_yard(_, [], _) :- retractall(visited(_, _)), findall(X, yard(Env, X), PlayPen), assertz(playpen(Env, PlayPen)), !.
generate_yard(Env, [(I, J) | Queue], NewQueue) :-
    expand(Env, [(I, J) | Queue], NewQueue),
    assertz(visited(Env, (I, J))),
    assertz(yard(Env, (I, J))).

generate_obstacle(Env) :-
    % findall(X, yard(Env, X), Yards),
    playpen(Env, Yards),
    indices(Env, Indices),
    subtract(Indices, Yards, Available),
    random_member(Elem, Available),
    assertz(obstacle(Env, Elem)).

generate_obstacles(Env, 0) :- findall(X, obstacle(Env, X), Obstacles), assertz(obstacles(Env, Obstacles)), !.
generate_obstacles(Env, Amount) :-
    generate_obstacle(Env),
    succ(NewAmount, Amount),
    generate_obstacles(Env, NewAmount).

generate_dirt(Env) :-
    % findall(X, yard(Env, X), Yards),
    % findall(Y, obstacle(Env, Y), Obstacles),
    playpen(Env, Yards),
    obstacles(Env, Obstacles),
    indices(Env, Indices),
    subtract(Indices, Yards, NoYards),
    subtract(NoYards, Obstacles, Available),
    random_member(Elem, Available),
    assertz(dirt(Env, Elem)).

generate_mess(Env, 0) :- findall(X, dirt(Env, X), Mess), assertz(mess(Env, Mess)), !.
generate_mess(Env, Amount) :-
    generate_dirt(Env),
    succ(NewAmount, Amount),
    generate_mess(Env, NewAmount).

generate_child(Env) :-
    % findall(X, yard(Env, X), Yards),
    % findall(Y, obstacle(Env, Y), Obstacles),
    playpen(Env, Yards),
    obstacles(Env, Obstacles),
    indices(Env, Indices),
    subtract(Indices, Yards, NoYards),
    subtract(NoYards, Obstacles, Available),
    random_member(Elem, Available),
    assertz(child(Env, Elem)).

generate_children(_, 0) :- findall(X, child(Env, X), Children), assertz(children(Env, Children)), !.
generate_children(Env, Amount) :-
    generate_child(Env),
    succ(NewAmount, Amount),
    generate_children(Env, NewAmount).

generate_robot(Env) :-
    % findall(X, yard(Env, X), Yards),
    % findall(Y, obstacle(Env, Y), Obstacles),
    % findall(Y, child(Env, Y), Children),
    playpen(Env, Yards),
    obstacles(Env, Obstacles),
    children(Env, Children),
    indices(Env, Indices),
    subtract(Indices, Yards, NoYards),
    subtract(NoYards, Children, NoChildren),
    subtract(NoChildren, Obstacles, Available),
    random_member(Elem, Available),
    assertz(robot(Env, Elem)).

place_items(_, [], _, _).
place_items(Env, [(I, J), Indices], Mask, NewEnv) :-
    index(Env, I, J, Tuple),
    bitwise_or(Tuple, Mask, NewElem),
    replace(Env, I, J, NewElem, TempEnv),
    place_items(TempEnv, Indices, Mask, NewEnv).

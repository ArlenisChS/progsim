:- consult('generator.pl').
:- consult('simulation.pl').
:- dynamic(t_spline/1, t_limit/1).

% :- initialization(main).

% main :-
%     % N, M, DirtyPercent, ObstaclePercent, ChildCount, Environment
%     % current_prolog_flag(argv, [N, M, D, O, C]),
%     all(),
%     halt.
% main :-
%     halt(1).

all() :-     
    % writeln("Test correct_number_of_yards"),
    % correct_number_of_yards(5, 5, 0.3, 0.1, 7),
    % writeln("Passed 1"),
    % correct_number_of_yards(10, 6, 0.2, 0.15, 8),
    % writeln("Passed 2"),
    % correct_number_of_yards(2, 6, 0.15, 0.2, 3),
    % writeln("Passed 3"),
    % writeln("Passed correct_number_of_yards"),
    
    % writeln("Test correct_number_of_kids"),
    % correct_number_of_kids(5, 5, 0.3, 0.1, 7),
    % writeln("Passed 1"),
    % correct_number_of_kids(10, 6, 0.2, 0.15, 8),
    % writeln("Passed 2"),
    % correct_number_of_kids(2, 6, 0.15, 0.2, 3),
    % writeln("Passed 3"),
    % writeln("Passed correct_number_of_kids"),

    % writeln("Test correct_dirt_amount"),
    % correct_dirt_amount(5, 5, 0.3, 0.1, 7),
    % writeln("Passed 1"),
    % correct_dirt_amount(10, 6, 0.2, 0.15, 8),
    % writeln("Passed 2"),
    % correct_dirt_amount(2, 6, 0.15, 0.2, 3),
    % writeln("Passed 3"),
    % writeln("Passed correct_dirt_amount"),

    % writeln("Test correct_number_of_obstacles"),
    % correct_number_of_obstacles(5, 5, 0.3, 0.1, 7),
    % writeln("Passed 1"),
    % correct_number_of_obstacles(10, 6, 0.2, 0.15, 8),
    % writeln("Passed 2"),
    % correct_number_of_obstacles(2, 6, 0.15, 0.2, 3),
    % writeln("Passed 3"),
    % correct_number_of_obstacles(30, 30, 0.1, 0.1, 25),
    % writeln("Passed 4"),
    % writeln("Passed correct_number_of_obstacles"),

    % writeln("Test feasibility"),
    % generate(5, 5, 0.3, 0.1, 7, Env1),
    % feasible(Env1),
    % writeln("Passed 1"),
    % generate(10, 6, 0.2, 0.15, 8, Env2),
    % feasible(Env2),
    % writeln("Passed 2"),
    % generate(2, 6, 0.15, 0.2, 3, Env3),
    % feasible(Env3),
    % writeln("Passed 3"),
    % writeln("Passed feasibility"),

    % writeln("Test Shortest Path"),
    % % generate(5, 5, 0.3, 0.1, 7, Env4),
    % generate(5, 5, 0.1, 0.1, 3, Env4),
    % % feasible(Env4),
    % % writeln("parent"),
    % robot(_, Robot),
    % writeln(("Robot", Robot)),
    % assertz(parent((0, 0), Robot)),
    % writeln("bfs_shortest_path_dirt_kid"),
    % bfs_shortest_path_dirt_kid(Env4, [Robot], 25),
    % writeln("findall"),
    % findall(PATH123123, path(PATH123123), Path),
    % writeln(Path),
    % writeln("clean"),
    % retractall(path(_)),
    % retractall(robot(_, _)),
    % % retractall(yard(_, _)),
    % % listing(parent),
    % % listing(path),
    % % listing,
    % writeln("Passed 1"),
    % generate(10, 6, 0.15, 0.15, 4, Env5),
    % % feasible(Env5),
    % % writeln("parent"),
    % robot(_, Robot1),
    % writeln(("Robot1", Robot1)),
    % assertz(parent((0, 0), Robot1)),
    % writeln("bfs_shortest_path_dirt_kid"),
    % bfs_shortest_path_dirt_kid(Env5, [Robot1], 60),
    % % bfs_shortest_path_dirt_kid(Env5, [search_path, [(2, 2)]], 60),
    % writeln("findall"),
    % findall(PATH123123123, path(PATH123123123), Path2),
    % writeln(Path2),
    % writeln("clean"),
    % retractall(path(_)),
    % writeln("Passed 2"),
    % % generate(2, 6, 0.15, 0.2, 3, Env6),
    % % feasible(Env6),
    % % writeln("Passed 3"),
    % writeln("Passed Shortest Path").
    % generate(7, 7, 0.15, 0.2, 3, Env6),
    % assertz(caughtChild(0)), 
    % run_one_turn(Env6, _).
    % open("output.txt", write, Stream), close(Stream),
    % simulation_(7, 7, 0.15, 0.2, 3, 60, 120, _).
    % simulations(7, 7, 0.2, 0.2, 4, 50, 500, 50).
    writeln("Ambiente 1. Parámetros: N = 10, M = 10, Dirty = 20%, Obstacles = 10%, Children = 6, t = 50."),
    simulations(10, 10, 0.2, 0.1, 6, 50, 500, 30),    
    findall(W1, win(W1), Wins1), findall(L1, lose(L1), Loses1), findall(PC1, polluted_cells(PC1), Polluted1),
    findall(TL1, timeout(TL1), TimeLimit1),
    length(Wins1, WCount1), write("Le pagaron al robot: "), writeln(WCount1),
    length(Loses1, LCount1), write("El robot fue despedido: "), writeln(LCount1),
    length(TimeLimit1, TLCount1), write("Se acabo la simulacion: "), writeln(TLCount1),
    sum_list(Polluted1, Sum1), Average1 is Sum1 / 30, write("El promedio de sucias fue: "), writeln(Average1),
    retractall(win(_)), retractall(lose(_)), retractall(polluted_cells(_)), retractall(timeout(_)),

    writeln("Ambiente 2. Parámetros: N = 4, M = 6, Dirty = 10%, Obstacles = 15%, Children = 2, t = 5."),
    simulations(4, 6, 0.1, 0.15, 2, 5, 500, 30),
    findall(W2, win(W2), Wins2), findall(L2, lose(L2), Loses2), findall(PC2, polluted_cells(PC2), Polluted2),
    findall(TL2, timeout(TL2), TimeLimit2),
    length(Wins2, WCount2), write("Le pagaron al robot: "), writeln(WCount2),
    length(Loses2, LCount2), write("El robot fue despedido: "), writeln(LCount2),
    length(TimeLimit2, TLCount2), write("Se acabo la simulacion: "), writeln(TLCount2),
    sum_list(Polluted2, Sum2), Average2 is Sum2 / 30, write("El promedio de sucias fue: "), writeln(Average2),
    retractall(win(_)), retractall(lose(_)), retractall(polluted_cells(_)), retractall(timeout(_)).

    % retractall(caughtChild(_)),
    % assertz(caughtChild(0)),
    % run_one_turn(Env6, Env7),
    % printWorld(Env7).
    % open("output.txt", append, Stream), close(Stream).

    % writeln(X),
    % writeln(Y),
    % caughtChild(Z),
    % writeln(Z),
    % move_robot(Env6, _, 45),
    % retractall(caughtChild(_)).

% move_robot(Env1, Env1, 0).
% move_robot(Env1, Env3, N):-
%     robot(_, (X, Y)),
%     robot2(Env1, X, Y, Env2),
%     printWorld(Env2),
%     N2 is N-1,
%     move_robot(Env2, Env3, N2).


% simulation(_, _, _, _, _, _, 0, _):-!.
% simulation(N, M, Dirt_P, Obstacule_P, Child_C, T_spline, T_limit, Final_Map):-
%     simulation_by_splines(N, M, Dirt_P, Obstacule_P, Child_C, T_spline, T_limit, Final_Map), !.


% simulation_by_splines(N, M, Dirt_P, Obstacule_P, Child_C, T_spline, T_limit, FMap):-
%     generate(N, M, Dirt_P, Obstacule_P, Child_C, Map), run_one_simulation(Map2),
%     T1 is T_spline - 1, T2 is T_limit-1.


% t(_, _).

% rational(0.3)
correct_number_of_yards(N, M, D, O, C) :- 
    generate(N, M, D, O, C, E),
    % writeln("Generated"),
    indices(E, Indices),
    count_objects(E, Indices, (0, 0, 1, 0, 0), 0, Count),
    % writeln(("Count C", C, Count)),
    C =:= Count,
    %  writeln("Checked").
correct_number_of_yards(_, _, _, _, _).


correct_number_of_kids(N, M, D, O, C) :- 
    generate(N, M, D, O, C, E),
    indices(E, Indices),
    count_objects(E, Indices, (0, 0, 0, 1, 0), 0, Count),
    writeln(("Count C", Count, C)),
    C =:= Count, writeln("Checked").
correct_number_of_kids(_, _, _, _, _).

correct_dirt_amount(N, M, D, O, C) :- 
    generate(N, M, D, O, C, E),
    indices(E, Indices),
    count_objects(E, Indices, (1, 0, 0, 0, 0), 0, Count),
    Size is N * M,
    DirtCount is round(Size * D),
    % writeln(("DirtCount C", DirtCount, Count)),
    DirtCount =:= Count. 
    % writeln("Checked").
correct_dirt_amount(_, _, _, _, _).

correct_number_of_obstacles(N, M, D, O, C) :- 
    generate(N, M, D, O, C, E),
    indices(E, Indices),
    count_objects(E, Indices, (0, 1, 0, 0, 0), 0, Count),
    Size is N * M,
    ObstacleCount is round(Size * O),
    % writeln(("ObstacleCount C", ObstacleCount, Count)),
    ObstacleCount =:= Count. 
    % writeln("Checked").
correct_number_of_obstacles(_, _, _, _, _).

feasible(Env) :-
    indices(Env, Indices),
    check_overlap(Env, Indices, (0, 1, 0, 1, 0)),
    check_overlap(Env, Indices, (0, 1, 0, 0, 1)),
    check_overlap(Env, Indices, (0, 0, 0, 1, 1)),
    check_overlap(Env, Indices, (0, 1, 1, 0, 0)),
    check_overlap(Env, Indices, (1, 0, 1, 0, 0)),
    check_overlap(Env, Indices, (1, 1, 0, 0, 0)), !.

count_objects(_, [], _, Count, Count).
count_objects(Env, [(I , J) | Indices], Mask, Count, Result) :-
    index(Env, I, J, Tuple),
    bitwise_and(Tuple, Mask, (X1, X2, X3, X4, X5)),
    NewCount is Count + X1 + X2 + X3 + X4 + X5,
    count_objects(Env, Indices, Mask, NewCount, Result).

check_overlap(_, [], _).
check_overlap(Env, [(I , J) | Indices], Mask) :-
    index(Env, I, J, Tuple),
    bitwise_and(Tuple, Mask, (X1, X2, X3, X4, X5)),
    2 > X1 + X2 + X3 + X4 + X5, !,
    check_overlap(Env, Indices, Mask).

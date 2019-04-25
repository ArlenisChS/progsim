:- consult('generator.pl').

% :- initialization(main).

% main :-
%     % N, M, DirtyPercent, ObstaclePercent, ChildCount, Environment
%     % current_prolog_flag(argv, [N, M, D, O, C]),
%     all(),
%     halt.
% main :-
%     halt(1).

all() :-     
    writeln("Test correct_number_of_yards"),
    correct_number_of_yards(5, 5, 0.3, 0.1, 7),
    writeln("Passed 1"),
    correct_number_of_yards(10, 6, 0.2, 0.4, 8),
    writeln("Passed 2"),
    correct_number_of_yards(2, 6, 0.4, 0.2, 3),
    writeln("Passed 3"),
    writeln("Passed correct_number_of_yards"),
    
    writeln("Test correct_number_of_kids"),
    correct_number_of_kids(5, 5, 0.3, 0.1, 7),
    writeln("Passed 1"),
    correct_number_of_kids(10, 6, 0.2, 0.4, 8),
    writeln("Passed 2"),
    correct_number_of_kids(2, 6, 0.4, 0.2, 3),
    writeln("Passed 3"),
    writeln("Passed correct_number_of_kids"),

    writeln("Test correct_dirt_amount"),
    correct_dirt_amount(5, 5, 0.3, 0.1, 7),
    writeln("Passed 1"),
    correct_dirt_amount(10, 6, 0.2, 0.4, 8),
    writeln("Passed 2"),
    correct_dirt_amount(2, 6, 0.4, 0.2, 3),
    writeln("Passed 3"),
    writeln("Passed correct_dirt_amount"),

    writeln("Test correct_number_of_obstacles"),
    correct_number_of_obstacles(5, 5, 0.3, 0.1, 7),
    writeln("Passed 1"),
    correct_number_of_obstacles(10, 6, 0.2, 0.4, 8),
    writeln("Passed 2"),
    correct_number_of_obstacles(2, 6, 0.4, 0.2, 3),
    writeln("Passed 3"),
    writeln("Passed correct_number_of_obstacles"),

    writeln("Test feasibility"),
    generate(5, 5, 0.3, 0.1, 7, Env1),
    feasible(Env1),
    writeln("Passed 1"),
    generate(10, 6, 0.2, 0.4, 8, Env2),
    feasible(Env2),
    writeln("Passed 2"),
    generate(2, 6, 0.4, 0.2, 3, Env3),
    feasible(Env3),
    writeln("Passed 3"),
    writeln("Passed feasibility").
    


% rational(0.3)
correct_number_of_yards(N, M, D, O, C) :- 
    generate(N, M, D, O, C, E),
    indices(E, Indices),
    count_objects(E, Indices, (0, 0, 1, 0, 0), 0, Count),
    C =:= Count.

correct_number_of_kids(N, M, D, O, C) :- 
    generate(N, M, D, O, C, E),
    indices(E, Indices),
    count_objects(E, Indices, (0, 0, 0, 1, 0), 0, Count),
    C =:= Count.

correct_dirt_amount(N, M, D, O, C) :- 
    generate(N, M, D, O, C, E),
    indices(E, Indices),
    count_objects(E, Indices, (1, 0, 0, 0, 0), 0, Count),
    Size is N * M,
    DirtCount is round(Size * D),
    DirtCount =:= Count.

correct_number_of_obstacles(N, M, D, O, C) :- 
    generate(N, M, D, O, C, E),
    indices(E, Indices),
    count_objects(E, Indices, (0, 1, 0, 0, 0), 0, Count),
    Size is N * M,
    ObstacleCount is round(Size * O),
    ObstacleCount =:= Count.

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

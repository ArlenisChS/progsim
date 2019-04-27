:- consult('matrix.pl').
:- dynamic(visited/2, obstacle/2, parent/2, path/1, caughtChild/1).

kid_and_dirt(Env, (I, J)) :- 
    index(Env, I, J, Tuple),
    bitwise_and(Tuple, (1, 0, 0, 1, 0), (X1, X2, X3, X4, X5)),
    Sum is X1 + X2 + X3 + X4 + X5, 
    Sum == 2.

kid_and_yard(Env, (I, J)) :- 
    index(Env, I, J, Tuple),
    bitwise_and(Tuple, (0, 0, 1, 1, 0), (X1, X2, X3, X4, X5)),
    Sum is X1 + X2 + X3 + X4 + X5, 
    Sum == 2.

kid_and_with_child(Env, (I, J)) :- 
    caughtChild(1),
    % writeln((caughtChild(X), X, "AAAAAAAAAAAAaaa")), 
    index(Env, I, J, (_, _, 0, 1, _)).
    % bitwise_and(Tuple, (0, 0, 0, 1, 0), (X1, X2, X3, X4, X5)),
    % Sum is 1 + X1 + X2 + X3 + X4 + X5, 
    % Sum == 2.

mark([]).
mark([(I, J) | List]) :-
    assertz(visited(_, (I, J))),
    mark(List).

update_parents((I, J), Children) :-
    exclude(parent((I, J)), Children, NoParents),
    nth1(1, NoParents, (X, Y), Rest),
    assertz(parent((I, J), (X, Y))),
    update_parents((I, J), Rest).
update_parents(_, _).

%====================================================
%
%                 Yard Generator
%
%====================================================
bfs_generate_yard(Env, [(I, J)|Rest], Steps) :-
    retractall(visited(_, _)), retractall(parent(_, _)), retractall(path(_)),
    assertz(parent((0, 0), (I, J))), 
    bfs_generate_yard_clean(Env, [(I, J)|Rest], Steps), !.

bfs_generate_yard_clean(_, _, 0) :- retractall(visited(_, _)), retractall(parent(_, _)), !.
bfs_generate_yard_clean(_, [], _) :- retractall(visited(_, _)), retractall(parent(_, _)), !.
bfs_generate_yard_clean(Env, Queue, Steps) :-
    length(Queue, Length),
    Length > 0,
    S1 is Steps - 1,
    bfs_generate_yard_one_step(Env, Queue, NewQueue),
    bfs_generate_yard_clean(Env, NewQueue, S1).

bfs_generate_yard_one_step(_, [], _) :- !.
bfs_generate_yard_one_step(Env, [(I, J) | Queue], NewQueue) :-
    bfs_generate_yard_expand(Env, [(I, J) | Queue], NewQueue),
    assertz(yard(_, (I, J))).

bfs_generate_yard_expand(_, [], _) :- !.
bfs_generate_yard_expand(Env, [(I, J) | Queue], NewQueue) :-
    directions4(Dirs),
    neighborhood(Env, I, J, Dirs, Neighbors),
    exclude(visited(_), Neighbors, NotVisited),
    mark([(I, J) | NotVisited]),
    append(Queue, NotVisited, NewQueue).


%====================================================
%
%             Shortest Path to Yard
%
%====================================================
bfs_shortest_path_yard(Env, [(I, J)|Rest], Steps) :-
    retractall(visited(_, _)), retractall(parent(_, _)), retractall(path(_)),
    assertz(parent((0, 0), (I, J))),
    bfs_shortest_path_yard_clean(Env, [(I, J)|Rest], Steps), !.

bfs_shortest_path_yard_clean(_, _, 0) :- !.
bfs_shortest_path_yard_clean(_, [], _) :- retract(path(_)), assertz(path((0, 0))), !.
bfs_shortest_path_yard_clean(Env, Queue, Steps) :-
    length(Queue, Length),
    Length > 0,
    S1 is Steps - 1,
    bfs_shortest_path_yard_one_step(Env, Queue, NewQueue),
    bfs_shortest_path_yard_clean(Env, NewQueue, S1).

bfs_shortest_path_yard_one_step(_, [], _) :- !.
bfs_shortest_path_yard_one_step(Env, [(I, J) | Queue], NewQueue) :-
    index(Env, I, J, Tuple),
    bitwise_and(Tuple, (0, 0, 1, 1, 0), (Y1, Y2, Y3, Y4, Y5)),
    Sum is Y1 + Y2 + Y3 + Y4 + Y5, 
    Sum == 2, !,
    bfs_shortest_path_yard_expand(Env, [(I, J) | Queue], NewQueue), !.
bfs_shortest_path_yard_one_step(Env, [(I, J) | Queue], NewQueue) :-
    index(Env, I, J, Tuple),
    bitwise_and(Tuple, (0, 0, 1, 0, 0), (X1, X2, X3, X4, X5)),
    Sum is X1 + X2 + X3 + X4 + X5, 
    Sum \= 1, !,
    bfs_shortest_path_yard_expand(Env, [(I, J) | Queue], NewQueue).
bfs_shortest_path_yard_one_step(_, [(I, J) | _], _) :- 
    bfs_shortest_path_yard_build_path((I, J)).

bfs_shortest_path_yard_build_path((0, 0)) :- retract(path(_)), assertz(path((0, 0))), !.
bfs_shortest_path_yard_build_path((I, J)) :- 
    parent(Parent, (I, J)),
    asserta(path((I, J))),
    bfs_shortest_path_yard_build_path(Parent).

bfs_shortest_path_yard_expand(_, [], _) :- !.
bfs_shortest_path_yard_expand(Env, [(I, J) | Queue], NewQueue) :-
    directions4(Dirs),
    neighborhood(Env, I, J, Dirs, Neighbors),
    exclude(visited(_), Neighbors, NotVisited),
    % exclude(kid_and_with_child(Env), NotVisited, NotKidsnoyard),
    exclude(obstacle(_), NotVisited, NotObstacle),
    update_parents((I, J), NotObstacle),
    mark([(I, J) | NotVisited]),
    append(Queue, NotObstacle, NewQueue).

%====================================================
%
%             Shortest Path to Kid
%
%====================================================
bfs_shortest_path_kid(Env, [(I, J) | Rest], Steps) :- 
    % writeln("AAA"),
    retractall(visited(_, _)), retractall(parent(_, _)), retractall(path(_)),
    assertz(parent((0, 0), (I, J))),
    % writeln("AAA2"),
    bfs_shortest_path_kid_clean(Env, [(I, J) | Rest], Steps), !.

bfs_shortest_path_kid_clean(_, _, 0) :- !.
bfs_shortest_path_kid_clean(_, [], _) :- retract(path(_)), assertz(path((0, 0))), !.
    % writeln("Base 2"),
    % listing(parent),
    % findall(X, visited(_, X), Visited), writeln(("Visited", Visited)),
    % findall(Y, parent(Z, Y), Parent), writeln(("Parent", Parent)), !.
    % retractall(visited(_, _)), retractall(parent(_, _)), !.
bfs_shortest_path_kid_clean(Env, Queue, Steps) :-
    % writeln("AAA3"),
    length(Queue, Length),
    Length > 0,
    S1 is Steps - 1,
    % writeln("AAA4"),
    bfs_shortest_path_kid_one_step(Env, Queue, NewQueue),
    % writeln("AAA5"),
    % writeln(("AAA5.1", Queue, NewQueue, Steps)),
    bfs_shortest_path_kid_clean(Env, NewQueue, S1).

bfs_shortest_path_kid_one_step(_, [], _) :- !.
% bfs_shortest_path_kid_one_step(Env, [(I, J) | Queue], NewQueue) :-
%     index(Env, I, J, Tuple),
%     bitwise_and(Tuple, (0, 0, 0, 1, 0), (X1, X2, X3, X4, X5)),
%     Sum is X1 + X2 + X3 + X4 + X5, 
%     Sum \= 1, !,
%     bfs_shortest_path_kid_expand(Env, [(I, J) | Queue], NewQueue).
bfs_shortest_path_kid_one_step(Env, [(I, J) | Queue], NewQueue) :-
    % writeln("AAA6"),
    index(Env, I, J, Tuple),
    % index(Env, I, J, (_, _, 1, 1, _)),
    bitwise_and(Tuple, (0, 0, 1, 1, 0), (Y1, Y2, Y3, Y4, Y5)),
    Sum is Y1 + Y2 + Y3 + Y4 + Y5, 
    Sum == 2, !,
    % writeln("AAA7"),
    bfs_shortest_path_dirt_kid_expand(Env, [(I, J) | Queue], NewQueue).
bfs_shortest_path_kid_one_step(Env, [(I, J) | Queue], NewQueue) :-
    % writeln("AAA8"),
    index(Env, I, J, Tuple),    
    % index(Env, I, J, (_, _, _, 0, _)),    
    bitwise_and(Tuple, (0, 0, 0, 1, 0), (X1, X2, X3, X4, X5)),
    Sum is X1 + X2 + X3 + X4 + X5, 
    Sum == 0, !,
    % writeln("AAA9"),
    bfs_shortest_path_dirt_kid_expand(Env, [(I, J) | Queue], NewQueue).
bfs_shortest_path_kid_one_step(Env, [(I, J) | _], _) :- 
    % writeln("AAA10"),
    index(Env, I, J, (_, _, _, 1, _)),    
    bfs_shortest_path_kid_build_path((I, J)).

bfs_shortest_path_kid_build_path((0, 0)) :- retract(path(_)), assertz(path((0, 0))), !.
bfs_shortest_path_kid_build_path((I, J)) :- 
    % writeln("AAA11"),
    parent(Parent, (I, J)),
    % writeln(("Parent", Parent)),
    asserta(path((I, J))),
    % writeln("AAA12"),
    bfs_shortest_path_kid_build_path(Parent).

bfs_shortest_path_kid_expand(_, [], _) :- !.
bfs_shortest_path_kid_expand(Env, [(I, J) | Queue], NewQueue) :-
    % writeln("AAA13"),
    directions4(Dirs),
    neighborhood(Env, I, J, Dirs, Neighbors),
    exclude(visited(_), Neighbors, NotVisited),
    % exclude(kid_and_dirt(Env), NotVisited, NotKidsDirt),
    exclude(obstacle(_), NotVisited, NotObstacle),
    update_parents((I, J), NotObstacle),
    % writeln("AAA14"),
    mark([(I, J) | NotVisited]),
    % writeln("AAA15"),
    append(Queue, NotObstacle, NewQueue).

%====================================================
%
%             Shortest Path to Dirt
%
%====================================================
bfs_shortest_path_dirt(Env, [(I, J)|Rest], Steps) :- 
    retractall(visited(_, _)), retractall(parent(_, _)), retractall(path(_)),
    assertz(parent((0, 0), (I, J))),
    bfs_shortest_path_dirt_clean(Env, [(I, J)|Rest], Steps), !.

bfs_shortest_path_dirt_clean(_, _, 0) :- !.
bfs_shortest_path_dirt_clean(_, [], _) :- retract(path(_)), assertz(path((0, 0))), !.
bfs_shortest_path_dirt_clean(Env, Queue, Steps) :-
    length(Queue, Length),
    Length > 0,
    S1 is Steps - 1,
    bfs_shortest_path_dirt_one_step(Env, Queue, NewQueue),
    bfs_shortest_path_dirt_clean(Env, NewQueue, S1).

bfs_shortest_path_dirt_one_step(_, [], _) :- !.
bfs_shortest_path_dirt_one_step(Env, [(I, J) | Queue], NewQueue) :-
    index(Env, I, J, Tuple),
    bitwise_and(Tuple, (1, 0, 0, 0, 0), (X1, X2, X3, X4, X5)),
    Sum is X1 + X2 + X3 + X4 + X5, 
    Sum \= 1, !,
    bfs_shortest_path_dirt_expand(Env, [(I, J) | Queue], NewQueue).
bfs_shortest_path_dirt_one_step(_, [(I, J) | _], _) :- 
    bfs_shortest_path_dirt_build_path((I, J)).

bfs_shortest_path_dirt_build_path((0, 0)) :- retract(path(_)), assertz(path((0, 0))), !.
bfs_shortest_path_dirt_build_path((I, J)) :- 
    parent(Parent, (I, J)),
    asserta(path((I, J))),
    bfs_shortest_path_dirt_build_path(Parent).

bfs_shortest_path_dirt_expand(_, [], _) :- !.
bfs_shortest_path_dirt_expand(Env, [(I, J) | Queue], NewQueue) :-
    directions4(Dirs),
    neighborhood(Env, I, J, Dirs, Neighbors),
    exclude(visited(_), Neighbors, NotVisited),
    % exclude(kid_and_dirt(Env), NotVisited, NotKidsDirt),
    exclude(obstacle(_), NotVisited, NotObstacle),
    update_parents((I, J), NotObstacle),
    mark([(I, J) | NotVisited]),
    append(Queue, NotObstacle, NewQueue).

%====================================================
%
%           Shortest Path to Dirt or Kid
%
%====================================================
bfs_shortest_path_dirt_kid(Env, [(I, J)|Rest], Steps) :-
    % writeln((Env, [(I, J)|Rest], Steps)),
    retractall(visited(_, _)), retractall(parent(_, _)), retractall(path(_)),
    assertz(parent((0, 0), (I, J))), 
    bfs_shortest_path_dirt_kid_clean(Env, [(I, J)|Rest], Steps), !.

bfs_shortest_path_dirt_kid_clean(_, _, 0) :- !.
bfs_shortest_path_dirt_kid_clean(_, [], _) :- retract(path(_)), assertz(path((0, 0))), !.
bfs_shortest_path_dirt_kid_clean(Env, Queue, Steps) :-
    length(Queue, Length),
    Length > 0,
    S1 is Steps - 1,
    bfs_shortest_path_dirt_kid_one_step(Env, Queue, NewQueue),
    bfs_shortest_path_dirt_kid_clean(Env, NewQueue, S1).

bfs_shortest_path_dirt_kid_one_step(_, [], _) :- !.
bfs_shortest_path_dirt_kid_one_step(Env, [(I, J) | Queue], NewQueue) :-
    index(Env, I, J, Tuple),
    bitwise_and(Tuple, (0, 0, 1, 1, 0), (Y1, Y2, Y3, Y4, Y5)),
    Sum is Y1 + Y2 + Y3 + Y4 + Y5, 
    Sum == 2, !,
    bfs_shortest_path_dirt_kid_expand(Env, [(I, J) | Queue], NewQueue), !.
bfs_shortest_path_dirt_kid_one_step(Env, [(I, J) | Queue], NewQueue) :-
    index(Env, I, J, Tuple),
    bitwise_and(Tuple, (1, 0, 0, 1, 0), (X1, X2, X3, X4, X5)),
    Sum is X1 + X2 + X3 + X4 + X5, 
    Sum == 0, !,
    bfs_shortest_path_dirt_kid_expand(Env, [(I, J) | Queue], NewQueue).
bfs_shortest_path_dirt_kid_one_step(_, [(I, J) | _], _) :- 
    bfs_shortest_path_dirt_kid_build_path((I, J)).

bfs_shortest_path_dirt_kid_build_path((0, 0)) :- retract(path(_)), assertz(path((0, 0))), !.
bfs_shortest_path_dirt_kid_build_path((I, J)) :- 
    parent(Parent, (I, J)),
    asserta(path((I, J))),
    bfs_shortest_path_dirt_kid_build_path(Parent).

bfs_shortest_path_dirt_kid_expand(_, [], _) :- !.
bfs_shortest_path_dirt_kid_expand(Env, [(I, J) | Queue], NewQueue) :-
    directions4(Dirs),
    neighborhood(Env, I, J, Dirs, Neighbors),
    exclude(visited(_), Neighbors, NotVisited),
    % exclude(kid_and_dirt(Env), NotVisited, NotKidsDirt),
    % exclude(kid_and_yard(Env), NotVisited, NotKidsYard),
    % exclude(kid_and_with_child(Env), NotVisited, NotKidsYard),
    exclude(obstacle(_), NotVisited, NotObstacle),
    update_parents((I, J), NotObstacle),
    mark([(I, J) | NotVisited]),
    append(Queue, NotObstacle, NewQueue).


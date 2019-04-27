:- consult('matrix.pl').
:- dynamic(visited/2, obstacle/2, parent/2, path/1).

kid_and_dirt(Env, (I, J)) :- 
    index(Env, I, J, Tuple),
    bitwise_and(Tuple, (1, 0, 0, 1, 0), (X1, X2, X3, X4, X5)),
    Sum is X1 + X2 + X3 + X4 + X5, 
    Sum == 2.

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

bfs_generate_yard(_, _, 0) :- retractall(visited(_, _)), !.
bfs_generate_yard(_, [], _) :- retractall(visited(_, _)), !.
bfs_generate_yard(Env, Queue, Steps) :-
    length(Queue, Length),
    Length > 0,
    S1 is Steps - 1,
    bfs_generate_yard_one_step(Env, Queue, NewQueue),
    bfs_generate_yard(Env, NewQueue, S1).

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

bfs_shortest_path(_, _, 0) :- retractall(visited(_, _)), !.
bfs_shortest_path(_, [], _) :- retractall(visited(_, _)), !.
bfs_shortest_path(Env, Queue, Steps) :-
    length(Queue, Length),
    Length > 0,
    S1 is Steps - 1,
    bfs_shortest_path_one_step(Env, Queue, NewQueue),
    bfs_shortest_path(Env, NewQueue, S1).

bfs_shortest_path_one_step(_, [], _) :- !.
bfs_shortest_path_one_step(Env, [(I, J) | Queue], NewQueue) :-
    index(Env, I, J, Tuple),
    bitwise_and(Tuple, (0, 0, 1, 0, 0), (X1, X2, X3, X4, X5)),
    Sum is X1 + X2 + X3 + X4 + X5, 
    Sum \= 1, !,
    bfs_shortest_path_expand(Env, [(I, J) | Queue], NewQueue).
bfs_shortest_path_one_step(_, [(I, J) | _], _) :- 
    bfs_shortest_path_build_path((I, J)).

bfs_shortest_path_build_path((0, 0)) :- retract(path(_)), assertz(path((0, 0))), !.
bfs_shortest_path_build_path((I, J)) :- 
    parent(Parent, (I, J)),
    asserta(path(((I, J)))),
    bfs_shortest_path_build_path(Parent).

bfs_shortest_path_expand(_, [], _) :- !.
bfs_shortest_path_expand(Env, [(I, J) | Queue], NewQueue) :-
    directions4(Dirs),
    neighborhood(Env, I, J, Dirs, Neighbors),
    exclude(visited(_), Neighbors, NotVisited),
    exclude(kid_and_dirt(Env), NotVisited, NotKidsDirt),
    exclude(obstacle(_), NotKidsDirt, NotObstacle),
    update_parents((I, J), NotObstacle),
    mark([(I, J) | NotVisited]),
    append(Queue, NotObstacle, NewQueue).



:- consult('matrix.pl').
:- dynamic(visited/2, obstacle/2, parent/2, path/1).

expand(_, [], []) :- !.
expand(Env, [(I, J) | Queue], NewQueue) :-
    directions4(Dirs),
    neighborhood(Env, I, J, Dirs, Neighbors),
    exclude(visited(Env), Neighbors, NotVisited),
    exclude(obstacle(Env), NotVisited, NotObstacle),
    update_parents((I, J), NotObstacle),
    % exclude(yard(Env), NotVisited, NotYard),
    % exclude(obstacle(Env), NotYard, NotObstacle),
    ord_union(Queue, NotObstacle, NewQueue).

higher_order_bfs(_, [BFS, _], 0) :- call(BFS, _, [], _), !.
higher_order_bfs(Env, [BFS, Queue], Steps) :- 
    S1 is Steps - 1,
    call(BFS, Env, Queue, NewQueue),
    higher_order_bfs(Env, [BFS, NewQueue], S1).


% Bfs that searches for the closest yard from a start
% Env   => The map
% Queue => Current state of the queue
% NewQueue => Next state of the queue
% generate_yard(Env, Queue, NewQueue)
search_path(_, [], _) :-
    retractall(visited(_, _)), !.
search_path(Env, [(I, J) | Queue], NewQueue) :-
    writeln("AAAAAA2"),
    index(Env, I, J, Tuple),
    bitwise_and(Tuple, (0, 0, 1, 0, 0), (X1, X2, X3, X4, X5)),
    Sum is X1 + X2 + X3 + X4 + X5, 
    Sum =\= 1, !,
    expand2(Env, [(I, J) | Queue], NewQueue),
    assertz(visited(Env, (I, J))).
search_path(_, [(I, J) | _], _) :- 
    writeln("AAAAAA3"),
    % listing(parent),
    % findall((X, Y), parent(X, Y), Parents),
    % writeln(Parents),
    build_path((I, J)).

expand2(_, [], []) :- !.
expand2(Env, [(I, J) | Queue], NewQueue) :-
    directions4(Dirs),
    neighborhood(Env, I, J, Dirs, Neighbors),
    exclude(visited(Env), Neighbors, NotVisited),
    exclude(obstacle(Env), NotVisited, NotObstacle),
    update_parents((I, J), NotObstacle),
    % exclude(yard(Env), NotVisited, NotYard),
    % exclude(obstacle(Env), NotYard, NotObstacle),
    ord_union(Queue, NotObstacle, NewQueue).

build_path((0, 0)) :- assertz(path((0, 0))), !.
build_path((I, J)) :- 
    writeln("BBBBBBB"),
    writeln((I, J)),
    writeln("CHILD"),
    % findall(X, parent((I, J), X), Children),
    % listing(parent),
    % writeln(Children),
    % fail,
    parent(Parent, (I, J)),
    writeln(Parent),
    writeln("PARENT"),
    assertz(path(Parent)),
    build_path(Parent).

update_parents((I, J), Children) :-
    % writeln(Children),
    exclude(parent((I, J)), Children, NoParents),
    % writeln(NoParents),
    nth1(1, NoParents, (X, Y), Rest),
    % writeln(Rest),
    assertz(parent((I, J), (X, Y))),
    update_parents((I, J), Rest).
update_parents(_, _).

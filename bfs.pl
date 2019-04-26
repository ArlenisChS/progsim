:- consult('matrix.pl').
:- dynamic(visited/2, obstacle/2).

expand(_, [], []) :- !.
expand(Env, [(I, J) | Queue], NewQueue) :-
    directions4(Dirs),
    neighborhood(Env, I, J, Dirs, Neighbors),
    exclude(visited(Env), Neighbors, NotVisited),
    exclude(obstacle(Env), NotVisited, NotObstacle),
    % exclude(yard(Env), NotVisited, NotYard),
    % exclude(obstacle(Env), NotYard, NotObstacle),
    ord_union(Queue, NotObstacle, NewQueue).

higher_order_bfs(_, [BFS, _], 0) :- call(BFS, _, [], _), !.
higher_order_bfs(Env, [BFS, Queue], Steps) :- 
    S1 is Steps - 1,
    call(BFS, Env, Queue, NewQueue),
    higher_order_bfs(Env, [BFS, NewQueue], S1).
:- consult('matrix.pl').
:- dynamic(visited/2, obstacle/2).

expand(_, [], []).
expand(Env, [(I, J) | Queue], NewQueue) :-
    directions8(Dirs),
    neighborhood(Env, I, J, Dirs, Neighbors),
    exclude(visited(Env), Neighbors, NotVisited),
    exclude(obstacle(Env), NotVisited, NotObstacle),
    % exclude(yard(Env), NotVisited, NotYard),
    % exclude(obstacle(Env), NotYard, NotObstacle),
    ord_union(Queue, NotObstacle, NewQueue).

higher_order_bfs(_, _, 0) :- !.
higher_order_bfs(Env, BFS, -1) :- higher_order_bfs(Env, BFS, 100000).
higher_order_bfs(Env, [BFS, Queue], Steps) :- 
    S1 is Steps - 1,
    call(BFS, Env, Queue, NewQueue),
    higher_order_bfs(Env, [BFS, NewQueue], S1).


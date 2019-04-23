% n - Row count
% m - Columns count
% dirty - Percent of dirty
% obstacles - Obstacle percent
% children - Children count
 
% generate(N, M, Dirty, Obstacles, Children) :-
%     empty(N, M, Env).

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


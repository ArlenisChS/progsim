:- consult('generator.pl').
:- consult('agent_robot.pl').
:- consult('agent_child.pl').
:- dynamic(t_spline/1, t_limit/1, win/1, lose/1, polluted_cells/1).

simulations(_, _, _, _, _, _, _, 0) :- !.
simulations(N, M, Dirt_P, Obstacule_P, Child_C, T_spline, T_limit, Count):-
    % open("output.txt", append, Stream), 
    % close(Stream),
    simulation_(N, M, Dirt_P, Obstacule_P, Child_C, T_spline, T_limit, _),
    NewCount is Count-1,
    simulations(N, M, Dirt_P, Obstacule_P, Child_C, T_spline, T_limit, NewCount).

simulation_(N, M, Dirt_P, Obstacule_P, Child_C, T_spline, T_limit, Map):-
    retractall(t_limit(_)), retractall(t_spline(_)),
    assertz(t_limit(T_limit)), assertz(t_spline(T_spline)),
    prepare_new(N, M, Dirt_P, Obstacule_P, Child_C, Map1),
    simulate_(N, M, Dirt_P, Obstacule_P, Child_C, 0, Map1, Map).    

simulate_(_, _, _, _, _, _,Map, Map) :-
    final_state(Map), !.
simulate_(_, _, _, _, _, N, Map, Map) :-
    t_limit(N), !, assertz(timeout(1)), 
    count_dirty(Map, Count),
    rows(Map, R), columns(Map, C),
    Size is R * C,
    Percent is (100 * Count) / Size,
    assertz(polluted_cells(Percent)).
simulate_(N, M, Dirt_P, Obstacule_P, Child_C, 0, Map1, MapF):-
    run_one_turn(Map1, Map2), 
    % NewTime is Time + 1,
    % writeln("Simulate Case 2 - Before simulate_ recursive call"),
    simulate_(N, M, Dirt_P, Obstacule_P, Child_C, 1, Map2, MapF), !.
simulate_(N, M, Dirt_P, Obstacule_P, Child_C, Time, _, MapF):-
    t_spline(T), Time mod T =:= 0, !,
    % writeln("Simulate Case 0 - Before prepare_new"),
    prepare_new(N, M, Dirt_P, Obstacule_P, Child_C, Map1),
    % writeln("Simulate Case 1 - Before rune_onte_turn"),
    run_one_turn(Map1, Map2), 
    NewTime is Time + 1,
    % writeln("Simulate Case 2 - Before simulate_ recursive call"),
    simulate_(N, M, Dirt_P, Obstacule_P, Child_C, NewTime, Map2, MapF).
simulate_(N, M, Dirt_P, Obstacule_P, Child_C, Time, Map, MapF):-
    run_one_turn(Map, Map2), 
    NewTime is Time + 1,
    simulate_(N, M, Dirt_P, Obstacule_P, Child_C, NewTime, Map2, MapF), !.


prepare_new(N, M, Dirt_P, Obstacule_P, Child_C, Map):-
%    open("output.txt", append, Stream), writeln(Stream, "Nuevo Mapa"), 
%     close(Stream),    
    % writeln("New Simulation"),
    generate(N, M, Dirt_P, Obstacule_P, Child_C, Map).    

run_one_turn(Map1, Map2):-
    % printWorld(Map1), nl,
    run_turn_over_kids(Map1, Map3), 
    % writeln("Termina con ninnos"),
    run_turn_over_robot(Map3, Map2).
    % writeln("Termina con robot").

run_turn_over_robot(Map1, Map2):-
    robot(_, (X, Y)),
    % caughtChild(X1),
    % writeln(("CC", X1)),
    % writeln(("LALALA", (X, Y))),
    robot1(Map1, X, Y, Map2).
    % printWorld(Map2).

run_turn_over_kids(Map1, Map2):-
    findall(K, child(_, K), KidsList),
    % writeln(KidsList),
    retractall(child(_, _)),
    run_turn_over_every_kid(Map1, KidsList, Map2).

run_turn_over_every_kid(Map1, [], Map1) :- !.
run_turn_over_every_kid(Map1, [(X, Y)|Rest], Map2):-
    % writeln("akiii"),
    index(Map1, X, Y, (_, _, 1, 1, _)), !,
    run_turn_over_every_kid(Map1, Rest, Map2), !.
run_turn_over_every_kid(Map1, [(X, Y)|Rest], Map2):-
    % writeln("akiii"),
    child(Map1, X, Y, Map3),
    % printWorld(Map3),
    run_turn_over_every_kid(Map3, Rest, Map2), !.


simulations1(_, _, _, _, _, _, _, 0) :- !.
simulations1(N, M, Dirt_P, Obstacule_P, Child_C, T_spline, T_limit, Count):-
    % open("output.txt", append, Stream), writeln(Stream, "New Simulation"),
    % close(Stream),
    simulation_1(N, M, Dirt_P, Obstacule_P, Child_C, T_spline, T_limit, _),
    NewCount is Count-1,
    simulations1(N, M, Dirt_P, Obstacule_P, Child_C, T_spline, T_limit, NewCount).

simulation_1(N, M, Dirt_P, Obstacule_P, Child_C, T_spline, T_limit, Map):-
    retractall(t_limit(_)), retractall(t_spline(_)),
    assertz(t_limit(T_limit)), assertz(t_spline(T_spline)),
    prepare_new(N, M, Dirt_P, Obstacule_P, Child_C, Map1),
    simulate_1(N, M, Dirt_P, Obstacule_P, Child_C, 0, Map1, Map).    

simulate_1(_, _, _, _, _, _,Map, Map) :-
    final_state(Map), !.
simulate_1(_, _, _, _, _, N, Map, Map) :-
    t_limit(N), !, assertz(timeout(1)), 
    count_dirty(Map, Count),
    rows(Map, R), columns(Map, C),
    Size is R * C,
    Percent is (100 * Count) / Size,
    assertz(polluted_cells(Percent)).
simulate_1(N, M, Dirt_P, Obstacule_P, Child_C, 0, Map1, MapF):-
    run_one_turn1(Map1, Map2), 
    % NewTime is Time + 1,
    % writeln("Simulate Case 2 - Before simulate_1 recursive call"),
    simulate_1(N, M, Dirt_P, Obstacule_P, Child_C, 1, Map2, MapF), !.
simulate_1(N, M, Dirt_P, Obstacule_P, Child_C, Time, _, MapF):-
    t_spline(T), Time mod T =:= 0, !,
    % writeln("Simulate Case 0 - Before prepare_new1"),
    prepare_new1(N, M, Dirt_P, Obstacule_P, Child_C, Map1),
    % writeln("Simulate Case 1 - Before rune_onte_turn"),
    run_one_turn1(Map1, Map2), 
    NewTime is Time + 1,
    % writeln("Simulate Case 2 - Before simulate_1 recursive call"),
    simulate_1(N, M, Dirt_P, Obstacule_P, Child_C, NewTime, Map2, MapF).
simulate_1(N, M, Dirt_P, Obstacule_P, Child_C, Time, Map, MapF):-
    run_one_turn1(Map, Map2), 
    NewTime is Time + 1,
    simulate_1(N, M, Dirt_P, Obstacule_P, Child_C, NewTime, Map2, MapF), !.


prepare_new1(N, M, Dirt_P, Obstacule_P, Child_C, Map):-
%    open("output.txt", append, Stream), writeln(Stream, "Nuevo Mapa"), 
%     close(Stream),    
    generate(N, M, Dirt_P, Obstacule_P, Child_C, Map).    

run_one_turn1(Map1, Map2):-
    % printWorld(Map1),
    run_turn_over_kids1(Map1, Map3), 
    % writeln("Termina con ninnos"),
    run_turn_over_robot1(Map3, Map2).
    % writeln("Termina con robot").

run_turn_over_robot1(Map1, Map2):-
    robot(_, (X, Y)),
    % caughtChild(X1),
    % writeln(("CC", X1)),
    % writeln(("LALALA", (X, Y))),
    robot2(Map1, X, Y, Map2).
    % printWorld(Map2).

run_turn_over_kids1(Map1, Map2):-
    findall(K, child(_, K), KidsList),
    % writeln(KidsList),
    retractall(child(_, _)),
    run_turn_over_every_kid1(Map1, KidsList, Map2).

run_turn_over_every_kid1(Map1, [], Map1) :- !.
run_turn_over_every_kid1(Map1, [(X, Y)|Rest], Map2):-
    % writeln("akiii"),
    index(Map1, X, Y, (_, _, 1, 1, _)), !,
    run_turn_over_every_kid1(Map1, Rest, Map2), !.
run_turn_over_every_kid1(Map1, [(X, Y)|Rest], Map2):-
    % writeln("akiii"),
    child(Map1, X, Y, Map3),
    % printWorld(Map3),
    run_turn_over_every_kid1(Map3, Rest, Map2), !.



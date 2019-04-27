:- consult('simulation.pl').
:- dynamic(win/1, lose/1, polluted_cells/1, timeout/1).

% :- set_prolog_flag(verbose, silent).
:- initialization(main).

main :-
    % open("output.txt", write, Stream), close(Stream), 
    % N, M, DirtyPercent, ObstaclePercent, ChildCount, Environment  
    
    writeln("Ambiente 1. Parámetros: N = 10, M = 10, Dirty = 0%, Obstacles = 0%, Children = 0, t = 50."),
    run_simulation(10, 10, 0, 0, 1, 50, 500, 30),    
    
    writeln("Ambiente 2. Parámetros: N = 4, M = 6, Dirty = 10%, Obstacles = 15%, Children = 2, t = 5."),
    run_simulation(4, 6, 0.1, 0.15, 2, 5, 500, 30),
    
    writeln("Ambiente 3. Parámetros: N = 7, M = 8, Dirty = 30%, Obstacles = 40%, Children = 3, t = 10."),
    run_simulation(7, 8, 0.3, 0.4, 3, 10, 500, 30),

    writeln("Ambiente 4. Parámetros: N = 15, M = 15, Dirty = 40%, Obstacles = 50%, Children = 7, t = 100."),
    run_simulation(15, 15, 0.4, 0.5, 7, 100, 500, 30),

    writeln("Ambiente 5. Parámetros: N = 8, M = 4, Dirty = 50%, Obstacles = 18%, Children = 4, t = 35."),
    run_simulation(8, 4, 0.5, 0.18, 4, 35, 500, 30),
    
    writeln("Ambiente 6. Parámetros: N = 5, M = 5, Dirty = 5%, Obstacles = 20%, Children = 2, t = 15."),
    run_simulation(5, 5, 0.05, 0.2, 2, 15, 500, 30),

    writeln("Ambiente 7. Parámetros: N = 3, M = 9, Dirty = 18%, Obstacles = 25%, Children = 3, t = 40."),
    run_simulation(3, 9, 0.18, 0.25, 3, 40, 500, 30),

    writeln("Ambiente 8. Parámetros: N = 11, M = 6, Dirty = 25%, Obstacles = 225%, Children = 6, t = 65."),
    run_simulation(11, 6, 0.25, 0.225, 6, 65, 500, 30),

    writeln("Ambiente 9. Parámetros: N = 6, M = 7, Dirty = 15%, Obstacles = 30%, Children = 5, t = 7."),
    run_simulation(6, 7, 0.15, 0.3, 5, 7, 500, 30),

    writeln("Ambiente 10. Parámetros: N = 20, M = 20, Dirty = 22.5%, Obstacles = 5%, Children = 8, t = 100."),
    run_simulation(20, 20, 0.225, 0.05, 8, 100, 500, 30),

    writeln("Ambient 11. Parámetros: N = 10, M = 10, Dirty = 20%, Obstacles = 10%, Children = 6, t = 50."),
    run_simulation1(10, 10, 0.2, 0.1, 6, 50, 500, 30),    
 
    writeln("Ambiente 12. Parámetros: N = 4, M = 6, Dirty = 10%, Obstacles = 15%, Children = 2, t = 5."),
    run_simulation1(4, 6, 0.1, 0.15, 2, 5, 500, 30),
    
    writeln("Ambiente 13. Parámetros: N = 7, M = 8, Dirty = 30%, Obstacles = 40%, Children = 3, t = 10."),
    run_simulation1(7, 8, 0.3, 0.4, 3, 10, 500, 30),

    writeln("Ambiente 14. Parámetros: N = 15, M = 15, Dirty = 40%, Obstacles = 50%, Children = 7, t = 100."),
    run_simulation1(15, 15, 0.4, 0.5, 7, 100, 500, 30),

    writeln("Ambiente 15. Parámetros: N = 8, M = 4, Dirty = 50%, Obstacles = 18%, Children = 4, t = 35."),
    run_simulation1(8, 4, 0.5, 0.18, 4, 35, 500, 30),
    
    writeln("Ambiente 16. Parámetros: N = 5, M = 5, Dirty = 5%, Obstacles = 20%, Children = 2, t = 15."),
    run_simulation1(5, 5, 0.05, 0.2, 2, 15, 500, 30),

    writeln("Ambiente 17. Parámetros: N = 3, M = 9, Dirty = 18%, Obstacles = 25%, Children = 3, t = 40."),
    run_simulation1(3, 9, 0.18, 0.25, 3, 40, 500, 30),

    writeln("Ambiente 18. Parámetros: N = 11, M = 6, Dirty = 25%, Obstacles = 225%, Children = 6, t = 65."),
    run_simulation1(11, 6, 0.25, 0.225, 6, 65, 500, 30),

    writeln("Ambiente 19. Parámetros: N = 6, M = 7, Dirty = 15%, Obstacles = 30%, Children = 5, t = 7."),
    run_simulation1(6, 7, 0.15, 0.3, 5, 7, 500, 30),

    writeln("Ambiente 20. Parámetros: N = 20, M = 20, Dirty = 22.5%, Obstacles = 5%, Children = 8, t = 100."),
    run_simulation1(20, 20, 0.225, 0.05, 8, 100, 500, 30),

    halt.
main :-
    halt(1).

run_simulation(N, M, D, O, C, T, TB, Count) :-
    simulations(N, M, D, O, C, T, TB, Count),
    findall(W, win(W), Wins), findall(L, lose(L), Loses), findall(PC, polluted_cells(PC), Polluted),
    findall(TL, timeout(TL), TimeLimit),
    length(Wins, WCount), write("Le pagaron al robot: "), writeln(WCount),
    length(Loses, LCount), write("El robot fue despedido: "), writeln(LCount),
    length(TimeLimit, TLCount), write("Se acabo la simulacion: "), writeln(TLCount),
    sum_list(Polluted, Sum), Average is Sum / 30, write("El promedio de sucias fue: "), writeln(Average),
    retractall(win(_)), retractall(lose(_)), retractall(polluted_cells(_)), retractall(timeout(_)).

run_simulation1(N, M, D, O, C, T, TB, Count) :-
    simulations1(N, M, D, O, C, T, TB, Count),
    findall(W, win(W), Wins), findall(L, lose(L), Loses), findall(PC, polluted_cells(PC), Polluted),
    findall(TL, timeout(TL), TimeLimit),
    length(Wins, WCount), write("Le pagaron al robot: "), writeln(WCount),
    length(Loses, LCount), write("El robot fue despedido: "), writeln(LCount),
    length(TimeLimit, TLCount), write("Se acabo la simulacion: "), writeln(TLCount),
    sum_list(Polluted, Sum), Average is Sum / 30, write("El promedio de sucias fue: "), writeln(Average),
    retractall(win(_)), retractall(lose(_)), retractall(polluted_cells(_)), retractall(timeout(_)).

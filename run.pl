:- consult('simulation.pl').
:- dynamic(win/1, lose/1, polluted_cells/1).

% :- set_prolog_flag(verbose, silent).
:- initialization(main).

main :-
    % open("output.txt", write, Stream), close(Stream), 
    % N, M, DirtyPercent, ObstaclePercent, ChildCount, Environment  
    
    writeln("Ambiente 1. Parámetros: N = 10, M = 10, Dirty = 20%, Obstacles = 10%, Children = 6, t = 50."),
    simulations(10, 10, 0.2, 0.1, 6, 50, 500, 30),    
    findall(W1, win(W1), Wins1), findall(L1, lose(L1), Loses1), findall(PC1, polluted_cells(PC1), Polluted1),
    findall(TL1, t_limit(TL1), TimeLimit1),
    length(Wins1, WCount1), write("Le pagaron al robot: "), writeln(WCount1),
    length(Loses1, LCount1), write("El robot fue despedido: "), writeln(LCount1),
    length(TimeLimit1, TLCount1), write("Se acabo la simulacion: "), writeln(TLCount1),
    sum_list(Polluted1, Sum1), Average1 is Sum1 / 30, write("El promedio de sucias fue: "), writeln(Average1),
    retractall(win(_)), retractall(lose(_)), polluted_cells(_),
 
    writeln("Ambiente 2. Parámetros: N = 4, M = 6, Dirty = 10%, Obstacles = 15%, Children = 2, t = 5."),
    simulations(4, 6, 0.1, 0.15, 2, 5, 500, 30),
    findall(W2, win(W2), Wins2), findall(L2, lose(L2), Loses2), findall(PC2, polluted_cells(PC2), Polluted2),
    findall(TL2, t_limit(TL2), TimeLimit2),
    length(Wins2, WCount2), write("Le pagaron al robot: "), writeln(WCount2),
    length(Loses2, LCount2), write("El robot fue despedido: "), writeln(LCount2),
    length(TimeLimit2, TLCount2), write("Se acabo la simulacion: "), writeln(TLCount2),
    sum_list(Polluted2, Sum2), Average2 is Sum2 / 30, write("El promedio de sucias fue: "), writeln(Average2),
    retractall(win(_)), retractall(lose(_)), polluted_cells(_),
    
    writeln("Ambiente 3. Parámetros: N = 7, M = 8, Dirty = 30%, Obstacles = 40%, Children = 3, t = 10."),
    simulations(7, 8, 0.3, 0.4, 3, 10, 500, 30),
    findall(W3, win(W3), Wins3), findall(L3, lose(L3), Loses3), findall(PC3, polluted_cells(PC3), Polluted3),
    findall(TL3, t_limit(TL3), TimeLimit3),
    length(Wins3, WCount3), write("Le pagaron al robot: "), writeln(WCount3),
    length(Loses3, LCount3), write("El robot fue despedido: "), writeln(LCount3),
    length(TimeLimit3, TLCount3), write("Se acabo la simulacion: "), writeln(TLCount3),
    sum_list(Polluted3, Sum3), Average3 is Sum3 / 30, write("El promedio de sucias fue: "), writeln(Average3),
    retractall(win(_)), retractall(lose(_)), polluted_cells(_),

    writeln("Ambiente 4. Parámetros: N = 15, M = 15, Dirty = 40%, Obstacles = 50%, Children = 7, t = 100."),
    simulations(15, 15, 0.4, 0.5, 7, 100, 500, 30),
    findall(W4, win(W4), Wins4), findall(L4, lose(L4), Loses4), findall(PC4, polluted_cells(PC4), Polluted4),
    findall(TL4, t_limit(TL4), TimeLimit4),
    length(Wins4, WCount4), write("Le pagaron al robot: "), writeln(WCount4),
    length(Loses4, LCount4), write("El robot fue despedido: "), writeln(LCount4),
    length(TimeLimit4, TLCount4), write("Se acabo la simulacion: "), writeln(TLCount4),
    sum_list(Polluted4, Sum4), Average4 is Sum4 / 30, write("El promedio de sucias fue: "), writeln(Average4),
    retractall(win(_)), retractall(lose(_)), polluted_cells(_),

    writeln("Ambiente 5. Parámetros: N = 8, M = 4, Dirty = 50%, Obstacles = 18%, Children = 4, t = 35."),
    simulations(8, 4, 0.5, 0.18, 4, 35, 500, 30),
    findall(W5, win(W5), Wins5), findall(L5, lose(L5), Loses5), findall(PC5, polluted_cells(PC5), Polluted5),
    findall(TL5, t_limit(TL5), TimeLimit5),
    length(Wins5, WCount5), write("Le pagaron al robot: "), writeln(WCount5),
    length(Loses5, LCount5), write("El robot fue despedido: "), writeln(LCount5),
    length(TimeLimit5, TLCount5), write("Se acabo la simulacion: "), writeln(TLCount5),
    sum_list(Polluted5, Sum5), Average5 is Sum5 / 30, write("El promedio de sucias fue: "), writeln(Average5),
    retractall(win(_)), retractall(lose(_)), polluted_cells(_),
    
    writeln("Ambiente 6. Parámetros: N = 5, M = 5, Dirty = 5%, Obstacles = 20%, Children = 2, t = 15."),
    simulations(5, 5, 0.05, 0.2, 2, 15, 500, 30),
    findall(W6, win(W6), Wins6), findall(L6, lose(L6), Loses6), findall(PC6, polluted_cells(PC6), Polluted6),
    findall(TL6, t_limit(TL6), TimeLimit6),
    length(Wins6, WCount6), write("Le pagaron al robot: "), writeln(WCount6),
    length(Loses6, LCount6), write("El robot fue despedido: "), writeln(LCount6),
    length(TimeLimit6, TLCount6), write("Se acabo la simulacion: "), writeln(TLCount6),
    sum_list(Polluted6, Sum6), Average6 is Sum6 / 30, write("El promedio de sucias fue: "), writeln(Average6),
    retractall(win(_)), retractall(lose(_)), polluted_cells(_),

    writeln("Ambiente 7. Parámetros: N = 3, M = 9, Dirty = 18%, Obstacles = 25%, Children = 3, t = 40."),
    simulations(3, 9, 0.18, 0.25, 3, 40, 500, 30),
    findall(W7, win(W7), Wins7), findall(L7, lose(L7), Loses7), findall(PC7, polluted_cells(PC7), Polluted7),
    findall(TL7, t_limit(TL7), TimeLimit7),
    length(Wins7, WCount7), write("Le pagaron al robot: "), writeln(WCount7),
    length(Loses7, LCount7), write("El robot fue despedido: "), writeln(LCount7),
    length(TimeLimit7, TLCount7), write("Se acabo la simulacion: "), writeln(TLCount7),
    sum_list(Polluted7, Sum7), Average7 is Sum7 / 30, write("El promedio de sucias fue: "), writeln(Average7),
    retractall(win(_)), retractall(lose(_)), polluted_cells(_),

    writeln("Ambiente 8. Parámetros: N = 11, M = 6, Dirty = 25%, Obstacles = 225%, Children = 6, t = 65."),
    simulations(11, 6, 0.25, 0.225, 6, 65, 500, 30),
    findall(W8, win(W8), Wins8), findall(L8, lose(L8), Loses8), findall(PC8, polluted_cells(PC8), Polluted8),
    findall(TL8, t_limit(TL8), TimeLimit8),
    length(Wins8, WCount8), write("Le pagaron al robot: "), writeln(WCount8),
    length(Loses8, LCount8), write("El robot fue despedido: "), writeln(LCount8),
    length(TimeLimit8, TLCount8), write("Se acabo la simulacion: "), writeln(TLCount8),
    sum_list(Polluted8, Sum8), Average8 is Sum8 / 30, write("El promedio de sucias fue: "), writeln(Average8),
    retractall(win(_)), retractall(lose(_)), polluted_cells(_),

    writeln("Ambiente 9. Parámetros: N = 6, M = 7, Dirty = 15%, Obstacles = 30%, Children = 5, t = 7."),
    simulations(6, 7, 0.15, 0.3, 5, 7, 500, 30),
    findall(W9, win(W9), Wins9), findall(L9, lose(L9), Loses9), findall(PC9, polluted_cells(PC9), Polluted9),
    findall(TL9, t_limit(TL9), TimeLimit9),
    length(Wins9, WCount9), write("Le pagaron al robot: "), writeln(WCount9),
    length(Loses9, LCount9), write("El robot fue despedido: "), writeln(LCount9),
    length(TimeLimit9, TLCount9), write("Se acabo la simulacion: "), writeln(TLCount9),
    sum_list(Polluted9, Sum9), Average9 is Sum9 / 30, write("El promedio de sucias fue: "), writeln(Average9),
    retractall(win(_)), retractall(lose(_)), polluted_cells(_),

    writeln("Ambiente 10. Parámetros: N = 20, M = 20, Dirty = 22.5%, Obstacles = 5%, Children = 8, t = 100."),
    simulations(20, 20, 0.225, 0.05, 8, 100, 500, 30),
    findall(W10, win(W10), Wins10), findall(L10, lose(L10), Loses10), findall(PC10, polluted_cells(PC10), Polluted10),
    findall(TL10, t_limit(TL10), TimeLimit10),
    length(Wins10, WCount10), write("Le pagaron al robot: "), writeln(WCount10),
    length(Loses10, LCount10), write("El robot fue despedido: "), writeln(LCount10),
    length(TimeLimit10, TLCount10), write("Se acabo la simulacion: "), writeln(TLCount10),
    sum_list(Polluted10, Sum10), Average10 is Sum10 / 30, write("El promedio de sucias fue: "), writeln(Average10),
    retractall(win(_)), retractall(lose(_)), polluted_cells(_),



    writeln("Ambient11. Parámetros: N = 10, M = 10, Dirty = 20%, Obstacles = 10%, Children = 6, t = 50."),
    simulations1(10, 10, 0.2, 0.1, 6, 50, 500, 30),    
    findall(W11, win(W11), Wins11), findall(L11, lose(L11), Loses11), findall(PC11, polluted_cells(PC11), Polluted11),
    findall(TL11, t_limit(TL11), TimeLimit11),
    length(Wins11, WCount11), write("Le pagaron al robot: "), writeln(WCount11),
    length(Loses11, LCount11), write("El robot fue despedido: "), writeln(LCount11),
    length(TimeLimit11, TLCount11), write("Se acabo la simulacion: "), writeln(TLCount11),
    sum_list(Polluted11, Sum11), Average11 is Sum11 / 30, write("El promedio de sucias fue: "), writeln(Average11),
    retractall(win(_)), retractall(lose(_)), polluted_cells(_),
 
    writeln("Ambiente 2. Parámetros: N = 4, M = 6, Dirty = 10%, Obstacles = 15%, Children = 2, t = 5."),
    simulations1(4, 6, 0.1, 0.15, 2, 5, 500, 30),
    findall(W12, win(W12), Wins12), findall(L12, lose(L12), Loses12), findall(PC12, polluted_cells(PC12), Polluted12),
    findall(TL12, t_limit(TL12), TimeLimit12),
    length(Wins12, WCount12), write("Le pagaron al robot: "), writeln(WCount12),
    length(Loses12, LCount12), write("El robot fue despedido: "), writeln(LCount12),
    length(TimeLimit12, TLCount12), write("Se acabo la simulacion: "), writeln(TLCount12),
    sum_list(Polluted12, Sum12), Average12 is Sum12 / 30, write("El promedio de sucias fue: "), writeln(Average12),
    retractall(win(_)), retractall(lose(_)), polluted_cells(_),
    
    writeln("Ambiente 3. Parámetros: N = 7, M = 8, Dirty = 30%, Obstacles = 40%, Children = 3, t = 10."),
    simulations1(7, 8, 0.3, 0.4, 3, 10, 500, 30),
    findall(W13, win(W13), Wins13), findall(L13, lose(L13), Loses13), findall(PC13, polluted_cells(PC13), Polluted13),
    findall(TL13, t_limit(TL13), TimeLimit13),
    length(Wins13, WCount13), write("Le pagaron al robot: "), writeln(WCount13),
    length(Loses13, LCount13), write("El robot fue despedido: "), writeln(LCount13),
    length(TimeLimit13, TLCount13), write("Se acabo la simulacion: "), writeln(TLCount13),
    sum_list(Polluted13, Sum13), Average13 is Sum13 / 30, write("El promedio de sucias fue: "), writeln(Average13),
    retractall(win(_)), retractall(lose(_)), polluted_cells(_),

    writeln("Ambiente 4. Parámetros: N = 15, M = 15, Dirty = 40%, Obstacles = 50%, Children = 7, t = 100."),
    simulations1(15, 15, 0.4, 0.5, 7, 100, 500, 30),
    findall(W14, win(W14), Wins14), findall(L14, lose(L14), Loses14), findall(PC14, polluted_cells(PC14), Polluted14),
    findall(TL14, t_limit(TL14), TimeLimit14),
    length(Wins14, WCount14), write("Le pagaron al robot: "), writeln(WCount14),
    length(Loses14, LCount14), write("El robot fue despedido: "), writeln(LCount14),
    length(TimeLimit14, TLCount14), write("Se acabo la simulacion: "), writeln(TLCount14),
    sum_list(Polluted14, Sum14), Average14 is Sum14 / 30, write("El promedio de sucias fue: "), writeln(Average14),
    retractall(win(_)), retractall(lose(_)), polluted_cells(_),

    writeln("Ambiente 5. Parámetros: N = 8, M = 4, Dirty = 50%, Obstacles = 18%, Children = 4, t = 35."),
    simulations1(8, 4, 0.5, 0.18, 4, 35, 500, 30),
    findall(W15, win(W15), Wins15), findall(L15, lose(L15), Loses15), findall(PC15, polluted_cells(PC15), Polluted15),
    findall(TL15, t_limit(TL15), TimeLimit15),
    length(Wins15, WCount15), write("Le pagaron al robot: "), writeln(WCount15),
    length(Loses15, LCount15), write("El robot fue despedido: "), writeln(LCount15),
    length(TimeLimit15, TLCount15), write("Se acabo la simulacion: "), writeln(TLCount15),
    sum_list(Polluted15, Sum15), Average15 is Sum15 / 30, write("El promedio de sucias fue: "), writeln(Average15),
    retractall(win(_)), retractall(lose(_)), polluted_cells(_),
    
    writeln("Ambiente 6. Parámetros: N = 5, M = 5, Dirty = 5%, Obstacles = 20%, Children = 2, t = 15."),
    simulations1(5, 5, 0.05, 0.2, 2, 15, 500, 30),
    findall(W16, win(W16), Wins16), findall(L16, lose(L16), Loses16), findall(PC16, polluted_cells(PC16), Polluted16),
    findall(TL16, t_limit(TL16), TimeLimit16),
    length(Wins16, WCount16), write("Le pagaron al robot: "), writeln(WCount16),
    length(Loses16, LCount16), write("El robot fue despedido: "), writeln(LCount16),
    length(TimeLimit16, TLCount16), write("Se acabo la simulacion: "), writeln(TLCount16),
    sum_list(Polluted16, Sum16), Average16 is Sum16 / 30, write("El promedio de sucias fue: "), writeln(Average16),
    retractall(win(_)), retractall(lose(_)), polluted_cells(_),

    writeln("Ambiente 7. Parámetros: N = 3, M = 9, Dirty = 18%, Obstacles = 25%, Children = 3, t = 40."),
    simulations1(3, 9, 0.18, 0.25, 3, 40, 500, 30),
    findall(W17, win(W17), Wins17), findall(L17, lose(L17), Loses17), findall(PC17, polluted_cells(PC17), Polluted17),
    findall(TL17, t_limit(TL17), TimeLimit17),
    length(Wins17, WCount17), write("Le pagaron al robot: "), writeln(WCount17),
    length(Loses17, LCount17), write("El robot fue despedido: "), writeln(LCount17),
    length(TimeLimit17, TLCount17), write("Se acabo la simulacion: "), writeln(TLCount17),
    sum_list(Polluted17, Sum17), Average17 is Sum17 / 30, write("El promedio de sucias fue: "), writeln(Average17),
    retractall(win(_)), retractall(lose(_)), polluted_cells(_),

    writeln("Ambiente 8. Parámetros: N = 11, M = 6, Dirty = 25%, Obstacles = 225%, Children = 6, t = 65."),
    simulations1(11, 6, 0.25, 0.225, 6, 65, 500, 30),
    findall(W18, win(W18), Wins18), findall(L18, lose(L18), Loses18), findall(PC18, polluted_cells(PC18), Polluted18),
    findall(TL18, t_limit(TL18), TimeLimit18),
    length(Wins18, WCount18), write("Le pagaron al robot: "), writeln(WCount18),
    length(Loses18, LCount18), write("El robot fue despedido: "), writeln(LCount18),
    length(TimeLimit18, TLCount18), write("Se acabo la simulacion: "), writeln(TLCount18),
    sum_list(Polluted18, Sum18), Average18 is Sum18 / 30, write("El promedio de sucias fue: "), writeln(Average18),
    retractall(win(_)), retractall(lose(_)), polluted_cells(_),

    writeln("Ambiente 9. Parámetros: N = 6, M = 7, Dirty = 15%, Obstacles = 30%, Children = 5, t = 7."),
    simulations1(6, 7, 0.15, 0.3, 5, 7, 500, 30),
    findall(W19, win(W19), Wins19), findall(L19, lose(L19), Loses19), findall(PC19, polluted_cells(PC19), Polluted19),
    findall(TL19, t_limit(TL19), TimeLimit19),
    length(Wins19, WCount19), write("Le pagaron al robot: "), writeln(WCount19),
    length(Loses19, LCount19), write("El robot fue despedido: "), writeln(LCount19),
    length(TimeLimit19, TLCount19), write("Se acabo la simulacion: "), writeln(TLCount19),
    sum_list(Polluted19, Sum19), Average19 is Sum19 / 30, write("El promedio de sucias fue: "), writeln(Average19),
    retractall(win(_)), retractall(lose(_)), polluted_cells(_),

    writeln("Ambiente 10. Parámetros: N = 20, M = 20, Dirty = 22.5%, Obstacles = 5%, Children = 8, t = 100."),
    simulations1(20, 20, 0.225, 0.05, 8, 100, 500, 30),
    findall(W20, win(W20), Wins20), findall(L20, lose(L20), Loses20), findall(PC20, polluted_cells(PC20), Polluted20),
    findall(TL20, t_limit(TL20), TimeLimit20),
    length(Wins20, WCount20), write("Le pagaron al robot: "), writeln(WCount20),
    length(Loses20, LCount20), write("El robot fue despedido: "), writeln(LCount20),
    length(TimeLimit20, TLCount20), write("Se acabo la simulacion: "), writeln(TLCount20),
    sum_list(Polluted20, Sum20), Average20 is Sum20 / 30, write("El promedio de sucias fue: "), writeln(Average20),
    retractall(win(_)), retractall(lose(_)), polluted_cells(_),

    halt.
main :-
    halt(1).
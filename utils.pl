% Cartesian product
cartprod(S, Tuples) :-
    findall(R, cart(S, R), L),
    toTuples(L, Tuples).

cart([], []).
cart([[A | _] | T], [A | R]) :-
   cart(T, R).

cart([[_ | B] | T], R) :-
   cart([B | T], R).

toTuples([], []).
toTuples([[X, Y] | ListOfLists], ListOfTuples) :-
    toTuples(ListOfLists, TempTuples),
    append([(X, Y)], TempTuples, ListOfTuples).

% Range of integers between `Lower` and `Upper`.
range(Upper, Upper, [Upper]) :- !.
range(Lower, Upper, List) :-
    Upper > Lower,
    succ(Lower, NewLower),
    range(NewLower, Upper, NewList),
    append([Lower], NewList, List).

bitwise_and((X1, X2, X3, X4, X5), (B1, B2, B3, B4, B5), NewTuple) :-
    XB1 is X1 /\ B1,
    XB2 is X2 /\ B2,
    XB3 is X3 /\ B3,
    XB4 is X4 /\ B4,
    XB5 is X5 /\ B5,
    NewTuple = (XB1, XB2, XB3, XB4, XB5).


bitwise_or((X1, X2, X3, X4, X5), (B1, B2, B3, B4, B5), NewTuple) :-
    XB1 is X1 \/ B1,
    XB2 is X2 \/ B2,
    XB3 is X3 \/ B3,
    XB4 is X4 \/ B4,
    XB5 is X5 \/ B5,
    NewTuple = (XB1, XB2, XB3, XB4, XB5).

get_random_element([X], [], X).
get_random_element(List, Rest, Element) :- 
    length(List, L), random_between(1, L, C), 
    nth1(C, List, Element, Rest).

count_object(_, [], _, 0) :- !.
count_object(Env, [(X, Y) | List], Mask, Count) :-
    index(Env, X, Y, Elem),
    bitwise_and(Elem, Mask, (X1, X2, X3, X4, X5)),
    C1 is X1 + X2 + X3 + X4 + X5,
    count_object(Env, List, Mask, C2),
    Count is C1 + C2.

my_random8(A, B) :- 
    random_between(1, 8, C), directions8(Dirc), 
    nth1(C, Dirc, (A, B)).

my_random4(A, B) :- 
    random_between(1, 4, C), directions4(Dirc),
    nth1(C, Dirc, (A, B)).
    
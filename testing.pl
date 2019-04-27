:- dynamic(pred/1).
pred(1).
pred(2).

ex(L) :- exclude(pred(), [1,2,3,4], L).

:- assertz(pred(3)).
:- assertz(pred(3)).
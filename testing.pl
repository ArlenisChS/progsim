pred(1).
pred(2).

ex(L) :- exclude(pred(), [1,2,3,4], L).


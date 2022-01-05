% computes the last element of a list.
last([E],E).
last([_,E1|R],E) :- last([E1|R],E).

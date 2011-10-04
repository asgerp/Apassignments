% Authors: Kristoffer Cobley and Asger Pedersen
% natural numbers, represented using the successor definition.

succ(z).
succ(s(N)) :- succ(N).

% returns a natural number. We know it is cheating but

intToNat(0,z).
intToNat(I,s(N)) :-
    I > 0,
    I1 is I-1,
    intToNat(I1,N).

% give succesor/predecessor googeligog
getSucc(X,s(X)).
getPred(s(X),X).

% 
% transitive, get predecessor, call less until either one is z
less(z, s(X)).
less(X,Y) :- getPred(X,Z), getPred(Y,W), less(Z,W).

% returns an int.
% does not work in current form.
natToInt(N,I) :-
    N =:= intToNat(I),
    I1 is I+1,
    natToInt(N,I1).


checkset(X).


ismember(X,Y,Z).

union(X,Y,Z).

intersection(X,Y,Z).
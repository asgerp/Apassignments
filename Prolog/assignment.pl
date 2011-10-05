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



%checkset(X), Y U NO WORKY?
checkset([X,Y|[]]) :- succ(X), succ(Y), less(X,Y); less(Y,X).
checkset([X,Y|Z]) :- succ(X), succ(Y), checkset([X|Z]), checkset([Y|Z]). 


% ismember, worky maybe.
ismember(X,[X|Ys],Z) :- succ(X), checkset([X|Ys]).
ismember(X,[Y|Ys],Z) :- ismember(X,Ys,Z).

union(X,Y,Z).

intersection(X,Y,Z).
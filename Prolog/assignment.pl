% natural numbers, represented using the successor definition.

succ(0).
succ(s(N)) :- succ(N).
    

% returns an int.
% does not work in current form.
natToInt(N,I) :-
    N =:= intToNat(I),
    I1 is I+1,
    natToInt(N,I1).


% returns a natural number.

intToNat(0,0).
intToNat(I,s(N)) :- 
    I > 0,
    I1 is I-1,
    intToNat(I1,N).
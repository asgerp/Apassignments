:- use_module(library(plunit)).
% Authors: Kristoffer Cobley and Asger Pedersen
% natural numbers, represented using the successor definition.

% succ\1 is a term a natural number?
succ(z).
succ(s(N)) :- succ(N).

% returns true
:- begin_tests(succtest).
test(succ_true1):-
	succ(z).
% return true
test(succ_true2) :-
	succ(s(z)).
% returns X = z; X = s(z)...
test(succ_true3, [nondet]):-
	succ(X).
% return false
test(succ_fail, [fail]):-
	succ(a).
:- end_tests(succtest).

% give succesor/predecessor
% getsucc\2 give successor
% getpred\2 give predecessor
%getsucc(X,s(X)).
getpred(s(X), X).

:- begin_tests(getpredtest).

%returns true
test(getpred_true1):-
	getpred(s(z), z).

% returns true
test(getpred_true2) :-
	getpred(s(s(z)), s(z)).

% return false
test(getpred_fail, [fail]) :-
	getpred(z, s(z)).

% returns X = s(z)
test(getpred_var1, all(X = [s(z)])) :-
	getpred(s(s(z)), X).

% returns X = s(s(s(z)))
test(getpred_var2, all(X = [s(s(s(z)))] )) :-
	getpred(X, s(s(z))).

:- end_tests(getpredtest).



% less\2
% get predecessor, call less until either one is z 
less(z, s(X)).
less(X, Y) :-
	getpred(X, Z),
	getpred(Y, W),
	less(Z, W).

:- begin_tests(lesstest).

% returns true
test(less_true1) :-
	less(z, s(z)), !.
% returns true
test(less_true2) :-
	less(z, s(s(z))), !.
% returns true
test(less_true3) :-
	less(s(z), s(s(z))), !.
% returns false
test(less_false1, [fail]) :-
	less(s(z), z).
% returns false
test(less_false2) :-
	 \+ less(s(s(z)), z).

test(less_false3) :-
	\+ less(s(z),s(z)).

% returns X = z; X = s(z); X = s(s(z))
test(less_var1) :-
	findall(X, less(X, s(s(s(z)))), Xs),
	Xs == [z, s(z), s(s(z))].
% returna X = s(s(s(z)))
test(less_var2) :-
	findall(X, (less(s(s(z)), X), less(X, s(s(s(s(z)))))), Xs),
	Xs == [s(s(s(z)))].


:- end_tests(lesstest).


%checkset\3 if only one element, check that it is a natural number.
% else check that X is strictly less than Y and call checkset recursively on Y|Z.
checkset([X|[]]) :-
	succ(X).
checkset([X, Y|Z]) :-
	less(X, Y),
	checkset([Y|Z]).

:- begin_tests(checksettest).

% returns false
test(checkset_false, [fail]):-
	checkset([]).

% returns false
test(checkset_false, [fail]):-
	 checkset([z, s(s(z)), s(z), s(s(s(z)))]).

% returns true
test(checkset_true1):-
	 checkset([z]), !.

% returns true
test(checkset_true2):-
	checkset([z, s(z)]), !.

% returns true
test(checkset_true2):-
	checkset([z, s(z), s(s(s(z)))]), !.

% returns true
test(checkset_true3):-
	checkset([z, s(z), s(s(z)), s(s(s(z)))]), !.
% returns X = z; X = s(z)
test(checkset_var1) :-
	findall(X, checkset([X, s(s(z)), s(s(s(z)))]), Xs),
	Xs == [z, s(z)].

:- end_tests(checksettest).


% if we reach the empty list, X is not a member.
% if X is the head, then X is a member.
% if Y < X, and X is not the head, then X can still be in the sorted set, tail recursion.
% if X is not at the head, and X > Y, X is not in the set.  
ismember(X, [], no).
ismember(X, [X|Ys], yes).
ismember(X, [Y|Ys], Z) :-
	less(Y, X),
	ismember(X, Ys, Z).
ismember(X, [Y|Ys], no) :-
	less(X, Y).

:- begin_tests(ismembertest).

% returns Z = yes
test(ismember_yes1, all(Z = [yes])) :-
	ismember(s(z), [s(z), s(s(s(z)))], Z).
% returns Z = yes
test(ismember_yes2, all(Z = [yes])) :-
	ismember(s(s(s(z))), [s(z), s(s(s(z)))], Z).
% returns Z = yes
test(ismember_yes3, all(Z = [yes])) :-
	ismember(s(s(s(z))), [s(z), s(s(s(z))), s(s(s(s(z))))], Z).
% returns Z = yes
test(ismember_yes4, all(Z = [yes])) :-
	ismember(z, [z], Z).
% returns Z = z; Z = s(z); Z = s(s(s(z)))
test(ismember_yes4, all(Z = [z, s(z), s(s(s(z)))])) :-
	ismember(N, [z, s(z), s(s(s(z)))], yes).

% returns Z = no
test(ismember_no1, all(Z = [no])) :-
	ismember(s(s(s(z))), [s(z)], Z).
% returns Z = no
test(ismember_no2, all(Z = [no])) :-
	ismember(z, [s(z)], Z).
% returns Z = no
test(ismember_no3, all(Z = [no])) :-
	ismember(z, [], Z).
% returns N = s(s(z)); N = s(s(s(s(z))))
test(ismember_no4, all(N = [s(s(s(s(z)))), s(s(z))])) :-
	ismember(N, [z, s(z), s(s(s(z)))], no),less(N,s(s(s(s(s(z)))))).
% returns Z = no
test(ismember_no5, all(Z = [no])) :-
	ismember(s(s(s(s(z)))), [z, s(z), s(s(s(z)))], Z).

% returns N = s(z), Z = yes; N = s(s(s(z))), Z = yes;
% N = s(s(s(s(z)))), Z = no; N = s(s(z)), Z = no; N = z, Z = no
test(ismember_no_yes, all(N = [s(z), s(s(s(z))), s(s(s(s(z)))), s(s(z)), z])) :-
	ismember(N,[s(z),s(s(s(z)))],Z), less(N,s(s(s(s(s(z)))))).

% returns N = s(z), Z = yes; N = s(s(s(z))), Z = yes;
% N = s(s(s(s(z)))), Z = no; N = s(s(z)), Z = no; N = z, Z = no
test(ismember_no_yes, all(Z = [yes, yes, no, no, no])) :-
	ismember(N,[s(z),s(s(s(z)))],Z), less(N,s(s(s(s(s(z)))))).

:- end_tests(ismembertest).

% union
union([],[],[]).
union([X|Xs], [], [X|Z]) :-
	union(Xs, [], Z).
union([], [Y|Ys], [Y|Z]) :-
	union(Ys, [], Z).
union([X|Xs],[X|Ys],[X|Z]) :-
	union(Xs,Ys,Z). % same head.
union([X|Xs],[Y|Ys],[X|Z]) :-
	less(X,Y),
	union(Xs,[Y|Ys],Z). % X < Y
union([X|Xs],[Y|Ys],[Y|Z]) :-
	less(Y,X),
	union([X|Xs],Ys,Z). % Y > X


:- begin_tests(uniontest).
% returns X = [z]
test(union_true1, all(X = [[z]])) :-
	union([z],[z], X).

% returns X = [z, s(z), s(s(z)), s(s(s(z)))]
test(union_true2, all(X = [[z, s(z), s(s(z)), s(s(s(z)))]])) :-	
	union([z, s(z), s(s(z))], [s(z), s(s(z)), s(s(s(z)))], X).

% returns X = [z, s(z), s(s(z)), s(s(s(z))), s(s(s(s(z))))]
test(union_true3, all(X = [[z, s(z), s(s(z)), s(s(s(z))), s(s(s(s(z))))]])) :-	
	union([z, s(z), s(s(z))], [s(s(s(z))), s(s(s(s(z))))], X).

% returns X = [z, s(z), s(s(z)), s(s(s(z)))]
test(union_true4, all(X = [[z, s(z), s(s(z)), s(s(s(z)))]])) :-
	union([z, s(s(z))], [s(z), s(s(s(z)))], X).

% returns X = [z]
test(union_true5, all(X = [[z]])):-
	union([], [z], Z).

% returns X = [z]
test(union_true6, all(X = [[z]])):-
	union([z], [], Z).

% returns X [z, s(z), s(s(z))], X = [z, s(s(z))], X = [s(z), s(s(z))], X = [s(s(z))]
test(union_true7, all(X = [[z, s(z), s(s(z))], [z, s(s(z))], [s(z), s(s(z))], [s(s(z))]])) :-
	union(X, [z,s(z)], [z,s(z),s(s(z))]).

% returns X = [z, s(z)], X = [s(z)]
test(union_true8, all(X = [[z, s(z)], [s(z)]])) :-
	union([z], X, [z,s(z)]).

% returns false
test(union_fail1, [fail]) :-
	union([z, s(z)], [z], [z]).

% returns false
test(union_fail2, [fail]) :-
	union([], [], [z]).

:- end_tests(uniontest).

% intersect\3
intersect([X|Xs],[X|Ys],[X|Z]) :- % head is the same
	intersect(Xs,Ys,Z).
intersect([X|Xs],[Y|Ys],Z) :- % X < Y
	less(X,Y),
	intersect(Xs,[Y|Ys],Z).
intersect([X|Xs],[Y|Ys],Z) :- % X > Y
	less(Y,X),
	intersect([X|Xs],Ys,Z).
intersect([X|Xs], [], Z) :- % ???
	intersect(Xs, [], Z).
intersect([], [Y|Ys], Z) :-
	intersect(Ys, [], Z).
intersect([],[],[]). % profit

:-begin_tests(intersectiontest).

% returns X = [z]
test(intersection_true1, all(X = [[z]])) :-
	   intersection([z], [z], X).

% returns X = [z, s(z), s(s(z))]
test(intersection_true2, all(X = [[z, s(z), s(s(z))]])) :-
	intersection([z, s(z), s(s(z))], [z, s(z), s(s(z))], X).

% returns X = []
test(intersection_true3, all(X = [[]])) :-
	   intersection([z], [s(z)], X).

% returns X = []
test(intersection_true4, all(X = [[]])) :-
	intersection([], [z], X).

% returns X = []
test(intersection_true5, all(X = [[]])) :-
	   intersection([z], [], X).

% returns X = [[s(s(z))]]
test(intersection_true6, all(X = [[s(s(z))]])) :-
	intersection([z, s(s(z)), s(s(s(s(z))))], [s(z), s(s(z)), s(s(s(z)))], X).

% returns true
test(intersection_true7) :-
	intersection([], [], []).

% returns false
test(intersection_fail1, [fail]) :-
	intersection([z],[s(z)],[s(z)]).

% returns false
test(intersection_fail2, [fail]) :-
	intersection([z],[],[s(z)]).

:-end_tests(intersectiontest).

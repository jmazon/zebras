% -*- Prolog -*-

% Solving support: shaping a solution to the domain

shape(Dom,Sol) :-
    inner_length(Dom,N),
    length(Sol,N),
    length(Dom,F),
    maplist(place(F),Sol).

place(F,P) :- length(X,F), P =.. [place|X].

inner_length([feature(_,H)|T],N) :-
    length(H,N),
    \+ ( member(feature(_,X),T), length(X,N2), N2 \= N ).

% Solving: asserting a constraint by unification

apply(Dom,Sol,same(attr(position,Pos),A)) :-
    !,
    ( Pos = centre -> length(Sol,L), M is (L+1)//2 ;
      Pos = first -> M = 1 ;
      Pos = ends -> (M = 1 ; length(Sol,M) ) ;
      throw(invalid_position(Pos)) ),
    nth1(M,Sol,P),
    unify(Dom,P,A).
apply(Dom,Sol,same(A,attr(position,Pos))) :-
    !,
    apply(Dom,Sol,same(attr(position,Pos),A)).
apply(Dom,Sol,same(A,B)) :-
    !,
    member(P,Sol),
    unify(Dom,P,A),
    unify(Dom,P,B).
apply(Dom,Sol,seq(A,B)) :-
    !,
    nextto(Pa,Pb,Sol),
    unify(Dom,Pa,A),
    unify(Dom,Pb,B).
apply(Dom,Sol,neighbors(A,B)) :-
    !,
    ( apply(Dom,Sol,seq(A,B)) ;
      apply(Dom,Sol,seq(B,A)) ).
apply(Dom,Sol,ordered(S)) :- !,
    apply_list(Dom,Sol,S).
apply(_,_,Other) :- throw(invalid_constraint(Other)).

% Solving support: assert a subsequence of constraints

apply_list(Dom,[Sh|St],[Ah|At]) :- unify(Dom,Sh,Ah), apply_list(Dom,St,At).
apply_list(Dom,[_|St],As) :- apply_list(Dom,St,As).
apply_list(_,_,[]).

% Solving support: unify a single feature

unify(Dom,P,attr(F,V)) :-
    nth0(N,Dom,feature(F,_)), !,
    P =.. [place|Vs],
    nth0(N,Vs,V).
% dev debug helper: report erroneous features instead of failing
% unify(_,_,U) :- throw(unify(U)).

% Solving support: enforce the entire domain being represented

assert_domain(Dom,Sol) :-
    length(Dom,L),
    foreach(between(1,L,X),assert_feature(X,Dom,Sol)).
assert_feature(X,Dom,Sol) :-
    nth1(X,Dom,feature(_,Vs)),
    maplist(assert_value(X,Sol),Vs).
assert_value(X,Sol,V) :-
    member(P,Sol),
    P =.. PVs,
    nth0(X,PVs,V).

% Global solving wrapper

solve(puzzle(Dom,Constraints),Sol) :-
    shape(Dom,Sol),
    maplist(apply(Dom,Sol),Constraints),
    assert_domain(Dom,Sol).

#! /usr/bin/env swipl
% -*- Prolog -*-

nation(house(N,_,_,_,_),N).
color(house(_,C,_,_,_),C).
pet(house(_,_,P,_,_),P).
drink(house(_,_,_,D,_),D).
cigs(house(_,_,_,_,C),C).

c1(S) :- length(S,5).
c2(S) :- member(H,S), nation(H,england), color(H,red).
c3(S) :- member(H,S), nation(H,spain), pet(H,dog).
c4(S) :- member(H,S), drink(H,coffee), color(H,green).
c5(S) :- member(H,S), nation(H,ukraine), drink(H,tea).
c6(S) :- seq(S,ap(color,ivory),ap(color,green)).
c7(S) :- member(H,S), cigs(H,old_gold), pet(H,snails).
c8(S) :- member(H,S), cigs(H,kool), color(H,yellow).
c9(S) :- length(S,L), M is L // 2, nth0(M,S,H), drink(H,milk).
c10([H|_]) :- nation(H,norway).
c11(S) :- next_to(S,ap(cigs,chesterfield),ap(pet,fox)).
c12(S) :- next_to(S,ap(cigs,kool),ap(pet,horse)).
c13(S) :- member(H,S), cigs(H,lucky_strike), drink(H,orange_juice).
c14(S) :- member(H,S), nation(H,japan), cigs(H,parliament).
c15(S) :- next_to(S,ap(nation,norway),ap(color,blue)).

seq([L,R|_],ap(Pa,Va),ap(Pb,Vb)) :- call(Pa,L,Va), call(Pb,R,Vb).
seq([_|T],A,B) :- seq(T,A,B).

next_to(S,A,B) :- seq(S,A,B) ; seq(S,B,A).

puzzle(S) :- c1(S), c2(S), c3(S), c4(S), c5(S), c6(S), c7(S), c8(S),
             c9(S), c10(S), c11(S), c12(S), c13(S), c14(S), c15(S).

main :- puzzle(S), print(S), nl.
:- initialization(main,main).

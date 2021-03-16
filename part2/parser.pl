% -*- Prolog -*-

:- [library(dcg/basics)].

words([W|Ws]) --> word(W), blanks, !, words(Ws).
words([]) --> [].

punct(".:").
skip(",%").
whitespace(" \n").

word_complement(Cs) :-
    punct(P), skip(S), whitespace(W),
    string_concat(P,S,T), string_concat(T,W,Cs).

word(T) -->
    { word_complement(Cp) },
    string_without(Cp, Codes),
    { Codes \= [] },
    { read_codes(Codes,T) }.
word(T) --> [C], { punct(P), string_codes(P,Cs), member(C,Cs), atom_codes(T,[C]) }.
word(T) --> [C], { skip(S), string_codes(S,Cs), member(C,Cs) }, blanks, word(T).

read_codes(Codes,Number) :-
    catch(number_codes(Number,Codes),
          error(syntax_error(illegal_number),_),
          fail).
read_codes(Codes,Token) :- atom_codes(Atom,Codes), downcase_atom(Atom,Token).

domain([F|Fs]) --> feature(F), domain(Fs).
domain([]) --> [].
feature(feature(Name,Vs)) --> [Name,':'], values(Vs).

values([]) --> ['.'], !.
values([V|Vs]) --> [V], values(Vs).

domain_values(D,Vs) :- maplist(feature_values,D,Vss), append(Vss,Vs).
feature_values(feature(_,Vs),Vs).

connectors([left,right,next,neighbour,'.',somewhere,between,and]).
positions([centre,first,ends]).
relatives([youngest,oldest]).

:- dynamic subst/2.

cleanse(D,Ts,C) :-
    connectors(Cs),
    positions(Ps),
    relatives(Rs),
    findall(S,subst(S,_),Ss),
    domain_values(D,Vs),
    append([Cs,Ps,Rs,Vs,Ss],K),
    include(member_(K),Ts,Tmp),
    maplist(perform_subst,Tmp,C).
member_(L,E) :- member(E,L).
perform_subst(F,T) :- subst(F,T), ! ; F = T.

constraints(Dom,[C|Cs]) --> constraint(Dom,C), ['.'], constraints(Dom,Cs).
constraints(_,[]) --> [].

constraint(Dom,same(A,B)) --> attribute(Dom,A), attribute(Dom,B).
constraint(Dom,seq(A,B)) --> attribute(Dom,A), [left], attribute(Dom,B).
constraint(Dom,seq(A,B)) --> attribute(Dom,B), [right], attribute(Dom,A).
constraint(Dom,neighbors(A,B)) -->
    attribute(Dom,A),
    [N], { member(N,[next,neighbour]) },
    attribute(Dom,B).
constraint(Dom,ordered([A,B])) -->
    attribute(Dom,A), [somewhere,left], attribute(Dom,B).
constraint(Dom,ordered([A,B])) -->
    attribute(Dom,B), [somewhere,right], attribute(Dom,A).
constraint(Dom,ordered([A,B,C])) -->
    attribute(Dom,B),
    [somewhere,between],
    attribute(Dom,A),
    [and],
    attribute(Dom,C).

attribute(Dom,attr(age,Young)) --> [youngest], { feature_min(Dom,age,Young) }.
attribute(Dom,attr(age,Young)) --> [oldest], { feature_max(Dom,age,Young) }.
attribute(Dom,attr(F,Min)) --> [min(F)], { feature_min(Dom,F,Min) }.
attribute(Dom,attr(F,V)) --> [V], { member(feature(F,Vs),Dom), member(V,Vs) }.
attribute(_,attr(position,P)) --> [P], { positions(Ps), member(P,Ps) }.

feature_min(Dom,F,Min) :-
    member(feature(F,Vs),Dom),
    min_member(Min,Vs).
feature_max(Dom,F,Min) :-
    member(feature(F,Vs),Dom),
    max_member(Min,Vs).

read_zebra(FileName,puzzle(Dom,Constraints)) :-
    phrase_from_file(words(Words),FileName),
    phrase(domain(Dom),Words,Verbose),
    cleanse(Dom,Verbose,Terse),
    phrase(constraints(Dom,Constraints),Terse).

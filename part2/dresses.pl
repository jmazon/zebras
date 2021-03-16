#! /usr/bin/env swipl
% -*- Prolog -*-

:- [parser,solver].

subst(smallest,min(discount)).
subst(before,left).

main :-
    read_zebra("dresses.txt",Puzzle),
    solve(Puzzle,Sol),
    print(Sol), nl.
:- initialization(main,main).

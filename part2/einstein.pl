#! /usr/bin/env swipl
% -*- Prolog -*-

:- [parser,solver].

main :-
    read_zebra("einstein.txt",Puzzle),
    solve(Puzzle,Sol),
    print(Sol), nl.
:- initialization(main,main).

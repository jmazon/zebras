% -*- Prolog -*-

:- [parser,solver].

main :-
    read_zebra("donation.txt",Puzzle),
    solve(Puzzle,Sol),
    print(Sol), nl.
:- initialization(main,main).

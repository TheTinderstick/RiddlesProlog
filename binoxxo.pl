:- encoding(utf8).
:- use_module(library(clpfd)).

maxConsecutiveEnd([H1,H2,H3]) :-
    H1 in 0..1,
    H2 in 0..1,
    H3 in 0..1,
    sum([H1,H2,H3],#<,3),
    sum([H1,H2,H3],#>,0).

maxConsecutive([H1,H2,H3|T]) :-
    H1 in 0..1,
    H2 in 0..1,
    H3 in 0..1,
    T ins 0..1,
    sum([H1,H2,H3],#<,3),
    sum([H1,H2,H3],#>,0),
    length([H2,H3|T], Len),
    ( Len#=3 -> 
       	maxConsecutiveEnd([H2,H3|T]);
    	maxConsecutive([H2,H3|T])).
    
binoxxo_row(L) :-
    L ins 0..1,
    global_cardinality(L,[0-5,1-5]),
    maxConsecutive(L).

diff_all([]).
diff_all([H|T]) :-
    maplist(dif(H),T),
    diff_all(T).

binoxxo_riddle(L) :-
    length(L,10),
    append(L, Vs), 
    Vs ins 0..1,
    transpose(L,Lt),
    diff_all(L),
    maplist(binoxxo_row,L),
    diff_all(Lt),
    maplist(binoxxo_row,Lt).

problem1([[0,_,_,_,0,_,_,0,_,_],
          [_,_,_,_,0,_,_,_,_,1],
          [_,_,0,0,_,0,_,_,_,_],
          [_,_,_,_,_,1,_,0,_,1],
          [_,_,1,0,_,_,_,_,_,_],
          [0,_,_,_,_,_,_,_,0,_],
          [_,_,0,_,1,_,_,_,_,0],
          [1,_,_,_,0,_,_,_,_,_],
          [1,0,_,_,_,_,_,_,1,_],
          [_,0,_,_,_,_,_,_,_,_]]).

problem2([[1,0,0,1,1,0,0,1,1,0],
          [0,1,1,0,0,1,1,0,1,0],
          [0,1,1,0,1,0,1,0,0,1],
          [1,0,0,1,1,0,0,1,0,1],
          [0,1,1,0,0,1,0,1,1,0],
          [0,0,1,1,0,1,1,0,0,1],
          [1,1,0,0,1,0,1,0,1,0],
          [0,0,1,0,1,1,0,1,0,1],
          [1,0,0,1,0,1,0,1,0,1],
          [1,1,0,1,0,0,1,0,1,0]]).

printRow([]).
printRow([H|T]) :-
    ( 0 #= H -> write('o'); write('x') ),
    length(T,Len),
    ( 0 #< Len -> write(',');write('')),
    printRow(T).

printMat([]) :-
    nl.
printMat([H|T]) :-
    write('['), 
    printRow(H), 
    write(']'), nl,
    printMat(T).

solver(L) :-
    write('Binoxxo solver'), nl,
    binoxxo_riddle(L), 
    append(L,Ls),   
    label(Ls),
    printMat(L).

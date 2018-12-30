:- use_module(library(clpfd)).

/*
 * Solver for Battleships puzzle (Bimaru)
 *
 * https://en.wikipedia.org/wiki/Battleship_(puzzle)
 *
 * Battlefield:
 * 0 => water
 * 1 => single piece (U-Boot)
 * 2 => middle piece
 * 3 => end left
 * 4 => end top
 * 5 => end right
 * 6 => end bottom
 *
 * The whole Problem:
 * [ Battlefield, [ShipcountRows, ShipcountColumns], [CountOfShips] ]
 *
 * The counts have to be given completely.
 * The battlefield only partially. 
 *
 */

example1(
[
  [0,3,2,5,0,0,0,0],
  [0,0,0,0,0,0,0,0],
  [0,0,0,1,0,1,0,0],
  [0,0,0,0,0,0,0,0],
  [0,3,2,5,0,4,0,1],
  [0,0,0,0,0,2,0,0],
  [0,0,0,4,0,2,0,0],
  [3,5,0,6,0,6,0,0]
],
[
  [3,0,2,0,5,1,2,4], /* amount of ship pieces in rows */
  [1,3,2,5,0,5,0,1]  /* amount of ship pieces in columns */
],
[3,2,2,1] /* number of uboots, destroyers, cruiser, battleships */
).
example1(Expl) :-
    Expl = [Battlefield,[Rows,Cols],ShipCounts],
    example1(Battlefield,[Rows,Cols],ShipCounts).

example2(
[
  [_,_,_,_,_,_,_,_,_,_],
  [_,_,_,_,_,_,_,_,_,_],
  [_,_,_,_,_,_,_,_,_,_],
  [_,_,_,_,2,_,_,_,3,_],
  [_,_,_,_,_,_,_,_,_,_],
  [6,_,_,_,_,_,_,_,_,_],
  [_,_,_,_,_,_,_,_,_,_],
  [_,_,_,_,_,_,_,_,_,_],
  [_,_,_,_,_,_,_,_,_,_],
  [_,_,_,_,_,_,0,_,_,_]
],
[
  [1,0,1,6,1,2,2,2,3,2], /* amount of ship pieces in rows */
  [3,2,2,1,1,4,2,2,2,1]  /* amount of ship pieces in columns */
],
[4,3,2,1] /* number of uboots, destroyers, cruiser, battleships */
).
example2(Expl) :-
    Expl = [Battlefield,[Rows,Cols],ShipCounts],
    example2(Battlefield,[Rows,Cols],ShipCounts).

example3(
[
  [0,3,2,5,0,0,0,0],
  [0,0,0,0,0,0,0,0],
  [0,_,0,1,0,1,0,0],
  [0,0,0,0,0,0,0,0],
  [0,3,2,5,0,_,0,1],
  [0,0,0,0,0,2,0,0],
  [0,0,0,4,0,2,0,0],
  [3,5,0,6,0,6,0,0]
],
[
  [3,0,2,0,5,1,2,4], /* amount of ship pieces in rows */
  [1,3,2,5,0,5,0,1]  /* amount of ship pieces in columns */
],
[3,2,2,1] /* number of uboots, destroyers, cruiser, battleships */
).
example3(Expl) :-
    Expl = [Battlefield,[Rows,Cols],ShipCounts],
    example3(Battlefield,[Rows,Cols],ShipCounts).


/* Method to count ship parts in a list */
cntShipPartsAcc([],Acc,Acc).
cntShipPartsAcc([H|T],Acc,Cnt) :-
    H in 0..6, T ins 0..6,
    (H #\= 0 -> Acc1 #= Acc+1; Acc1 #= Acc),
    cntShipPartsAcc(T,Acc1,Cnt).

/* non-accumulator version */
cntShipParts(L,Cnt) :-
    cntShipPartsAcc(L,0,Cnt).

/* rule 1: count of ship parts correct for all rows
 */
checkShipPartCountRows([],[]).
checkShipPartCountRows([Bh|Bt],[Rh|Rt]) :-
    cntShipParts(Bh,Rh),
    checkShipPartCountRows(Bt,Rt).

checkShipPartCount(B,[R,C]) :-
    checkShipPartCountRows(B,R),
    transpose(B, Btrans),
    checkShipPartCountRows(Btrans,C).

/* Check if there is ship [Sh|St] in list [Lh|Lt] */
checkShipInList([],_).
checkShipInList([Sh|St],[Lh|Lt]) :-
    length([Lh|Lt],ListLen),
    length([Sh|St],ShipLen),
    ListLen #>= ShipLen,
    (Lh #= Sh -> 
      checkShipInList(St,Lt);
      checkShipInList([Sh|St],Lt)
    ).

/* Check if there is ship [Sh|St] next in list [Lh|Lt] */
checkShipNextInList([],_).
checkShipNextInList([Sh|St],[Lh|Lt]) :-
    length([Lh|Lt],ListLen),
    length([Sh|St],ShipLen),
    ListLen #>= ShipLen,
    (Lh #= Sh -> 
      checkShipNextInList(St,Lt);
      false
    ).

/* Ship definitions. Horizontal/vertical orientation
 * is different
 */
uboot([1]).
destroyerV([4,6]).
destroyerH([3,5]).
cruiserV([4,2,6]).
cruiserH([3,2,5]).
battleshipV([4,2,2,6]).
battleshipH([3,2,2,5]).

checkWaterH(0,_).
checkWaterH(1,0).
checkWaterH(3,5).
checkWaterH(3,2).
checkWaterH(4,0).
checkWaterH(2,5).
checkWaterH(2,0).
checkWaterH(2,2).
checkWaterH(5,0).
checkWaterH(6,0).

checkWaterV(0,_).
checkWaterV(1,0).
checkWaterV(3,0).
checkWaterV(4,2).
checkWaterV(4,6).
checkWaterV(2,0).
checkWaterV(2,6).
checkWaterV(2,2).
checkWaterV(5,0).
checkWaterV(6,0).

checkWaterD(0,_).
checkWaterD(_,0).

/* If orientation is not important */
destroyer(S) :- !; destroyerV(S); destroyerH(S).
cruiser(S) :- !; cruiserV(S); cruiserH(S).
battleship(S) :- !; battleshipV(S); battleshipH(S).

/* Count ships of type 'Ship' in List */
cntShipInListAcc(_,[],Acc,Acc).
cntShipInListAcc(Ship,[Lh|Lt],Acc,Cnt) :-
    (checkShipNextInList(Ship,[Lh|Lt]) -> 
      Acc1 #= Acc+1;
      Acc1 #= Acc),
    cntShipInListAcc(Ship,Lt,Acc1,Cnt).
cntShipInList(Ship,List,Cnt) :-
    cntShipInListAcc(Ship,List,0,Cnt).

/* Count ships on battlefield */
cntShipOnBattlefieldAcc(_,[],Acc,Acc).
cntShipOnBattlefieldAcc(Ship,[Bh|Bt],Acc,Cnt) :-
    cntShipInList(Ship,Bh,ListCnt),
    Acc1 #= Acc+ListCnt,
    cntShipOnBattlefieldAcc(Ship,Bt,Acc1,Cnt).
cntShipOnBattlefield(Ship,Battlefield,Cnt) :-
    cntShipOnBattlefieldAcc(Ship,Battlefield,0,Cnt).

/* Count UBoots in battlefield */
cntUboot(Battlefield,Cnt) :-
    uboot(UBoot),
    cntShipOnBattlefield(UBoot,Battlefield,Cnt).

/* Count destroyers on battlefield */
cntDestroyers(Battlefield,Cnt) :-
    destroyerH(DestroyerH),
    cntShipOnBattlefield(DestroyerH,Battlefield,CntH),
    transpose(Battlefield,Battlefield_),
    destroyerV(DestroyerV),
    cntShipOnBattlefield(DestroyerV,Battlefield_,CntV),
    Cnt #= CntH+CntV.

/* Count cruisers on battlefield */
cntCruisers(Battlefield,Cnt) :-
    cruiserH(CruiserH),
    cntShipOnBattlefield(CruiserH,Battlefield,CntH),
    transpose(Battlefield,Battlefield_),
    cruiserV(CruiserV),
    cntShipOnBattlefield(CruiserV,Battlefield_,CntV),
    Cnt #= CntH+CntV.

/* Count cruisers on battlefield */
cntBattleships(Battlefield,Cnt) :-
    battleshipH(BattleshipH),
    cntShipOnBattlefield(BattleshipH,Battlefield,CntH),
    transpose(Battlefield,Battlefield_),
    battleshipV(BattleshipV),
    cntShipOnBattlefield(BattleshipV,Battlefield_,CntV),
    Cnt #= CntH+CntV.

/* Rule 2: check count of all ship types */
checkShipCounts(Battlefield,Shipcounts) :-
    cntUboot(Battlefield,UBootCnt),
	cntDestroyers(Battlefield,DestCnt),
	cntCruisers(Battlefield,CruiserCnt),
	cntBattleships(Battlefield,BCnt),
    [UBootCnt,DestCnt,CruiserCnt,BCnt] == Shipcounts.

/* Create new Matrix from NxN Matrix ,
 * so that diagonals are put in rows.
 * Example:
 * [a, b, c]
 * [d, e, f]
 * [g, h, j]
 *     =>
 *    [a]
 *  [b, d]
 * [c, e, g]
 *  [f, h]
 *    [j]
 */
getDiagHeads([],[],Acc,Acc,ResultAcc,ResultAcc).
    /*print("3").*/
getDiagHeads(Row,[],Acc,Diag,ResultAcc,Result) :-
    /*print("2"),*/
    Row = [Value|RowRest],
    append(Acc,[Value],Diag),
    append(ResultAcc,[RowRest],Result).
getDiagHeads([],Rows,Acc,Diag,ResultAcc,Result) :-
    /*print("1"),*/
    Rows = [Row2|Rest],
    getDiagHeads(Row2,Rest,Acc,Diag,ResultAcc,Result).
getDiagHeads(Row1,Rows,Acc,Diag,ResultAcc,Result) :-
    /*print("0"),*/
    Row1 = [Value|Row1Rest],
    Rows = [Row2|Rest],
    append(Acc,[Value],Acc1),
    length(Row1,LenRow1),
    length(Row2,LenRow2),
    append(ResultAcc,[Row1Rest],ResultAcc1),
    ( LenRow1 #= LenRow2 ->  
      Diag = Acc1,
      append(ResultAcc1,Rows,Result);
      getDiagHeads(Row2,Rest,Acc1,Diag,ResultAcc1,Result) 
    ).

getDiagonalsAcc([],ResultAcc,ResultAcc).
getDiagonalsAcc(Mat,ResultAcc,Result) :-
    Mat = [R|Rest],
    getDiagHeads(R,Rest,[],Diag,[],Next),
    length(Diag,DiagLen),
    ( DiagLen #\= 0 ->  
      append(ResultAcc,[Diag],ResultAcc1);
      ResultAcc1 = ResultAcc
    ),
    getDiagonalsAcc(Next,ResultAcc1,Result).
getDiagonals(Mat,Result) :-
    getDiagonalsAcc(Mat,[],Result).

/* Test for water spaces in horizontal row */
checkRow([],_).
checkRow(List,Last) :-
    List = [Next|Rest],
    checkWaterH(Last,Next),
    checkRow(Rest,Next).
checkRow(List) :-
    List = [Next|Rest],
    checkRow(Rest,Next).

/* Test for water spaces in vertical row */
checkCol([],_).
checkCol(List,Last) :-
    List = [Next|Rest],
    checkWaterV(Last,Next),
    checkCol(Rest,Next).
checkCol(List) :-
    List = [Next|Rest],
	checkCol(Rest,Next).

/* Test for water spaces on diagonals */    
checkDiag([],_).
checkDiag(List,Last) :-
    List = [Next|Rest],
    checkWaterD(Last,Next),
    checkDiag(Rest,Next).
checkDiag(List) :-
    List = [Next|Rest],
    checkDiag(Rest,Next).

/* Check for water spaces in all rows of battlefield */
checkWaterRows([]).
checkWaterRows(Battlefield) :-
    Battlefield = [Row|Rest],
    checkRow(Row),
    checkWaterRows(Rest).

/* Check for water spaces in all columns of battlefield */
checkWaterCols([]).
checkWaterCols(Battlefield) :-
    Battlefield = [Col|Rest],
    checkCol(Col),
    checkWaterCols(Rest).

/* Check for water spaces on all diagonals of battlefield */    
checkWaterDiags([]).
checkWaterDiags(Battlefield) :-
    Battlefield = [Diag|Rest],
    checkDiag(Diag),
    checkWaterDiags(Rest).

checkWaterSpaces(Battlefield) :-
    checkWaterRows(Battlefield),
    getDiagonals(Battlefield,Diags),
    checkWaterDiags(Diags),
    transpose(Battlefield,Battlefield_),
    checkWaterCols(Battlefield_),
    getDiagonals(Battlefield_,Diags_),
    checkWaterDiags(Diags_).

/* The complete riddle:
 * rule 1: ship parts must be as given
 * rule 2: the count od the ship types must be as given
 * rule 3: the ships are not allowed to be directly adjacent
 */
bimaru_riddle(R) :-
    R = [Battlefield,[Rows,Cols],ShipCounts],
    append(Battlefield, Vs),
    Vs ins 0..6,
    checkShipPartCount(Battlefield,[Rows,Cols]),
    checkShipCounts(Battlefield,ShipCounts),
    checkWaterSpaces(Battlefield).

/* Helpers to print out the solution */
printBimaruRowInt([]).
printBimaruRowInt([H|T]) :-
    write(H),
    length(T,Len),
    ( 0 #< Len -> write(',');write('')),
    printBimaruRowInt(T).

printBimaruRow(R,RowCount) :-
    write("["),
    printBimaruRowInt(R),
    write("] - "),
    write(RowCount),
    nl.

printBimaruColumnCountInt1([]).
printBimaruColumnCountInt1([_H|T]) :-
    write("|"),
    length(T,Len),
    ( 0 #< Len -> write(' ');write('')),
    printBimaruColumnCountInt1(T).

printBimaruColumnCountInt2([]).
printBimaruColumnCountInt2([H|T]) :-
    write(H),
    length(T,Len),
    ( 0 #< Len -> write(' ');write('')),
    printBimaruColumnCountInt2(T).

printBimaruColumnCount(Cc) :-
    write(" "),
    printBimaruColumnCountInt1(Cc), nl,
    write(" "),
    printBimaruColumnCountInt2(Cc), nl.

printBimaruShipCountAcc([],_).
printBimaruShipCountAcc([H|T],Acc) :-
    write("Ships of length "), write(Acc), write(": "),
    write(H), nl,
    Acc1 is Acc+1,
    printBimaruShipCountAcc(T,Acc1).
printBimaruShipCount(Sc) :-
    printBimaruShipCountAcc(Sc,1).

printBimaru([],[[],Cc],Sc) :-
    printBimaruColumnCount(Cc),
    printBimaruShipCount(Sc).
printBimaru(B,[Rc,Cc],Sc) :-
    B = [Row|Rest],
    Rc = [RowCount|RestRowCount], 
    printBimaruRow(Row,RowCount), 
    printBimaru(Rest,[RestRowCount,Cc],Sc).
 
/* The solver.
 *
 * To solve an exmple do
 * example(E), solver(E).
 */   
solver(R) :-
    write('Bimaru solver'), nl,
    R = [B,[Rc,Cc],Sc], 
    append(B,Ls), 
    Ls ins 0..6,  
    label(Ls),
    bimaru_riddle(R),
    printBimaru(B,[Rc,Cc],Sc), nl.   
 

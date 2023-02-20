:- dynamic detailed_mode_disabled/0.
:- ensure_loaded('files.pl').
% student file for Ultimate Tic Tac Toe implementation

% initialState/1
% initialState(-State)
% Este adevărat pentru starea inițială a jocului.
%t state: board, last move, last player, UboardState
initialState(state([E,E,E,E,E,E,E,E,E], _, _, E)) :- empty_board(E).
initialState(_) :- false.

% getBoards/2
% getBoards(+State, -Boards)
% Este adevărat dacă în starea State, informațiile din tablele individuale sunt
% cele din variabila Boards.
% Boards este legată la o listă de 9 elemente, fiecare element reprezentând o tablă.
% Ordinea tablelor este cea din lista positions (din utils.pl).
% Fiecare element din listă este o listă de 9 elemente, reprezentând
% pozițiile de pe tablă, ca x, 0, sau ''.
% Pozițiile sunt în ordinea din lista positions (din utils.pl).
getBoards( state([A,B,C,D,E,F,G, H, I], _, _, _), [A,B,C,D,E,F,G, H, I] ) :- true, !.
getBoards(_, _) :- false.
% getBoard/3
% getBoard(+State, +UPos, -Board)
% Este adebărat dacă în starea State, la poziția UPos din tabla de UTTT, 
% se află tabla individuală cu reprezentarea din Board.
% Reprezentarea tablei este descrisă în predicatul getBoards/2.
getBoard(state(Boards, _, _, _), Upos, Board):-  positions(L) , nth0(Poz, L, Upos), nth0(Poz, Boards, Board).
getBoard(_, _, _) :- false.

% getUBoard/2
% getUBoard(stare(+Board, +UboardState, +Player, +NextMoves),
% -UboardState)
% Întoarce reprezentarea UBoard-ului, indicând tablele individuale câștigate,
% remizate, sau încă în desfășurare. Reprezentarea este aceeași ca a tablelor
% individuale (vezi getBoards/2).
myReverseAcc([], L, L).
myReverseAcc([H|T],A,L):- myReverseAcc(T,[H|A],L).

getUBoardhelper([], UboardState, UboardState).
getUBoardhelper([A|Rest], UboardState, Acc) :- getBoardResult(A, Rez), getUBoardhelper(Rest, UboardState, [Rez | Acc]). 
getUBoard(state(Boards, _, _,_), UboardState) :- getUBoardhelper(Boards, CUboardState,[]), myReverseAcc(CUboardState, [], UboardState).
getUBoard(_, _) :- false.

% getPos/4
% getPos(+State, +UPos, +Pos, -Cell).
% Este adevărat dacă în starea State, în tabla individuală de la poziția UPos în UBoard,
% la poziția Pos pe tablă, se află simbolul Cell (x, 0, sau '').
getPos(state(Boards, _, _, _), Upos, Pos, Cell) :- positions(L), nth0(Poz,L, Upos), nth0(Poz, Boards, Board), getPos(Board, Pos, Cell).
getPos(_, _, _, _) :- false.

% getPos/3
% getPos(+Board, +Pos, -Cell).
% Este adevărat dacă în tabla individuală reprezentată în Board, la poziția Pos, 
% se află simbolul Cell (x, 0, sau ''). Predicatul poate fi folosit și pentru UBoard, caz 
% în care Cell poate fi și r.
getPos(Board, Pos, Cell) :- positions(L), nth0(Poz, L, Pos) ,nth0(Poz, Board, Cell).
getPos(_, _, _) :- false.

% getNextPlayer/2
% getNextPlayer(+State), -NextPlayer)
% Este adevărat dacă în starea State, jucătorul care urmează este NextPlayer
% (poate fi x sau 0)..
%getNextPlayer(S, x) :- initialState(S), !.
%getNextPlayer(state(_,_,LastPlayer, _), NextPlayer) :- nextPlayer(LastPlayer, NextPlayer),!.
%getNextPlayer(_, _) :- false.

getNextPlayer(S,x) :- getNrx(S, Nrx), getNr0(S,Nr0), Nrx =:= Nr0, !.
getNextPlayer(S,0) :- getNrx(S, Nrx), getNr0(S,Nr0), Nrx > Nr0, !.

getNrx(S, Nr) :- findall((X,Y),(positions(L), member(X,L), member(Y, L),getPos(S,X,Y,x) ), Bag), length(Bag, Nr).

getNr0(S, Nr) :- findall((X,Y),(positions(L), member(X,L), member(Y, L),getPos(S,X,Y,0) ), Bag), length(Bag, Nr).

% getNextAvailableBoards/2
% getNextAvailableBoards(+State, -NextBoardsPoss)
% Este adevărat dacă în starea State, pozițiile din NextBoardsPoss sunt pozițiile 
% din UBoard ale tablelor disponibile pentru următoarea mutare.
getNextAvailableBoards(S, NextBoardsPoss) :- initialState(S), positions(NextBoardsPoss),!.
getNextAvailableBoards(state(Boards,LastMove,_,_), [LastMove]) :-  positions(L), nth0(Idx, L, LastMove), nth0(Idx,Boards, Board ), getBoardResult(Board, ''),  !.
getNextAvailableBoards(state(Boards, _, _, _), NextBoardsPoss) :- positions(L), findall(X, (member(X, L), nth0(Poz, L, X), nth0(Poz,Boards, Board), getBoardResult(Board, '')), NextBoardsPoss).
getNextAvailableBoards(_, _) :- false.

% getBoardResult/2
% getBoardResult(+Board, -Result)
% Este adevărat dacă pentru o tablă individuală (sau UBoard) cu reprezentarea
% din Board, rezultatul este Result. Result poate fi:
% x sau 0, dacă jucătorul respectiv a câștigat jocul pe tabla dată;
% r, dacă s-a ajuns la remiză (toate pozițiile au fost completate dar
% tabla nu a fost câștigată);
% '', dacă tabla nu a fost câștigată și nu s-au completat toate pozițiile.
% NOTĂ: este deja definit predicatul player_wins/2 în utils.pl.
getBoardResult(Board, Res) :- player_wins(Res, Board), !.
getBoardResult(Board, '') :- nth0(_, Board, ''), \+player_wins(_,Board), !.
getBoardResult(Board, r) :- \+nth0(_,Board,''), \+player_wins(_,Board), !.
getBoardResult(_, _) :- false.

% buildState/3
% buildState(+Boards, +PreviousPos, -State)
% Este adevărat dacă starea State corespunde stării jocului în care tablele
% individuale sunt cele din lista Boards, iar ultima mutare a fost în 
% poziția PreviousPos într-o tablă individuală.
% NOTĂ: nu contează în care tablă individuală s-a realizat ultima mutare.
buildState(Boards, PreviousPos, state(Boards, PreviousPos, _, _)) :- true.
buildState(_, _, _) :- false.

% validMove/2
% validMove(+State, +Move)
% Este adevărat dacă mutarea Move este legală în starea State.
% Move este fie o poziție, în cazul în care este o singură tablă disponibilă
% pentru a următoarea mutare din starea State, fie o pereche de poziții, altfel.
validMove(S, Move) :- getUBoard(S,UboardState), getBoardResult(UboardState, ''), getNextAvailableBoards(S, [Y]), getBoard(S,Y,Board), getPos(Board,Move, ''), !.
validMove(S, (Pos, Poz)) :- getUBoard(S,UboardState), getBoardResult(UboardState, ''), getNextAvailableBoards(S,NextBoardsPoss),  nth0(_, NextBoardsPoss, Pos), getBoard(S, Pos, Board), getPos(Board,Poz,'' ). 
validMove(_, _) :- false.

% makeMove/3
% makeMove(+State, +Move, -NewState)
% Este adevărat dacă în urma aplicării mutării Move în starea State
% rezulta starea NewState.
% Move este fie o poziție (din lista positions), în cazul în care nu sunt mai 
% multe table disponibile pentru a următoarea mutare din starea State,
% fie o pereche de poziții, altfel.
%
% Hint: folosiți validMove pentru a verifica mutarea și buildState pentru a construi o stare.
makeMove(state(Boards,LastMove, LastPlayer, UboardState), (MoveU, Move), state(NewBoards, Move, Player, UboardState)) :- validMove(state(Boards,LastMove, LastPlayer, UboardState), (MoveU, Move)), getNextPlayer(state(Boards,LastMove, LastPlayer, UboardState), Player) , positions(L),modifyBoards(Boards, MoveU, Move, Player,L,[], NewBoards),!. 
makeMove(state(Boards,LastMove, LastPlayer,UboardState), Move, state(NewBoards, Move, Player, UboardState)) :- validMove(state(Boards,LastMove, LastPlayer,UboardState), Move), getNextPlayer(state(Boards,LastMove, LastPlayer, UboardState), Player), positions(L), modifyBoards(Boards, LastMove, Move, Player, L,[], NewBoards), !. 
makeMove(_, _, _) :- false.

%modifyBoards(+Boards, +Upos, +Pos, +Cell, Positions, +Acc, -NewBoards)

modifyBoards([], _, _, _, _, Acc, NewBoards) :- myReverseAcc(Acc, [], NewBoards).
modifyBoards([A|Rest], Upos, Pos, Cell, [Upos | RestPos], Acc, NewBoards) :- positions(L), modifyBoard(A, Pos, Cell,L, [],NewBoard) ,modifyBoards(Rest, Upos, Pos, Cell, RestPos, [NewBoard | Acc], NewBoards), !.
modifyBoards([A|Rest], Upos, Pos, Cell, [_|RestPos], Acc, NewBoards) :- modifyBoards(Rest, Upos, Pos, Cell, RestPos, [A | Acc], NewBoards).
%modifyBoard(+Board,+Pos, +Cell,+Positions, +Acc,-NewBoard )
modifyBoard([],_, _, _, Acc, NewBoard) :- myReverseAcc(Acc, [], NewBoard).
modifyBoard([_|Rest], Pos, Cell, [Pos|Pozs], Acc, NewBoard) :- modifyBoard(Rest, Pos, Cell, Pozs, [Cell | Acc], NewBoard), !.
modifyBoard([A|Rest], Pos, Cell, [_|Pozs], Acc, NewBoard) :- modifyBoard(Rest, Pos, Cell, Pozs, [A | Acc], NewBoard). 


% dummy_first/2
% dummy_first(+State, -NextMove)
% Predicatul leagă NextMove la următoarea mutare pentru starea State.
% Strategia este foarte simplă: va fi aleasă cea mai din stânga-sus mutare posibilă
% (prima din lista de poziții disponibile).
dummy_first(S,(nw,nw)) :- initialState(S).
dummy_first(state(Boards,LastMove, _,_), NextMove) :- getBoard(state(Boards,LastMove, _,_), LastMove, Board), getBoardResult(Board, ''), positions(L), findall(X, (member(X, L), validMove(state(Boards,LastMove, _,_), X)), Bag), nth0(0, Bag, NextMove), !. 
dummy_first(State, NextMove) :- positions(L), findall((X,Y), (member(X, L), member(Y,L), validMove(State, (X,Y))), Bag), nth0(0, Bag, NextMove).
dummy_first(_, _) :- false.

% dummy_last/2
% dummy_last(+State, -NextMove)
% Predicatul leagă NextMove la următoarea mutare pentru starea State.
% Strategia este foarte simplă: va fi aleasă cea mai din dreapta-jos mutare posibilă 
% (ultima din lista de poziții disponibile).
dummy_last(S,(se,se)) :- initialState(S).
dummy_last(state(Boards,LastMove, _,_), NextMove) :- getBoard(state(Boards,LastMove, _,_), LastMove, Board), getBoardResult(Board, ''), positions(L), findall(X, (member(X, L), validMove(state(Boards,LastMove, _,_), X)), Bag), myReverseAcc(Bag,[], RevBag), nth0(0, RevBag, NextMove), !. 
dummy_last(State, NextMove) :- positions(L), findall((X,Y), (member(X, L), member(Y,L), validMove(State, (X,Y))), Bag), myReverseAcc(Bag,[], RevBag), nth0(0, RevBag, NextMove). 
dummy_last(_, _) :- false.

:- module(numbersGame,
        [ winnerNumber/1,
          possible/1,
          data/3
        ]).

:- use_module(accessories).
:- use_module(common).

%==================================================================
% Data
%==================================================================
numberSize(4).
% data(Number,Perfect Qty, Just-Fine Qty).
/*data([1,2,3,4],0,2).
data([7,8,9,0],0,1).
data([5,3,2,9],1,0).
data([6,3,1,7],2,2).*/
/*data([3,8,5,0],0,1).
data([7,1,2,6],1,1).
data([1,5,6,4],1,0).
data([7,0,6,9],1,1).*/
/*data([8,7,6,5],0,1).
data([0,9,4,3],0,2).
data([1,8,3,4],0,2).
data([4,1,5,0],3,0).*/
/*data([6,2,9,4],1,0).
data([6,3,1,0],0,2).
data([0,2,7,1],0,1).
data([3,0,9,8],2,1).
data([3,0,8,4],1,1).*/
/*data([7,5,4,3],0,1).
data([2,6,9,1],0,2).
data([1,0,5,8],0,3).
data([1,2,8,5],1,3).*/
data([4,3,2,1],0,1).
data([9,0,7,8],0,2).
data([7,4,0,5],0,1).
data([6,8,3,0],3,0).
data([6,8,1,0],2,0).


%==================================================================
% Main Logic
%==================================================================
winnerNumber(N):- justOne(possible), possible(N).

possible(N):-
            validNumber(N),
            not(discarded(N)).

%=============
% Possible Number Discard
%=============

% Every data must be checked with the number.
% The number is discarded if it doesn't respect one data number's Perfect and Fine quantities.
discarded(N):-
               data(DataNumber,_,_),not(checks(N,DataNumber)).
checks(N,DataNumber):-
                data(DataNumber,PerfectQty,FineQty),
                count(fineDigit(N,DataNumber),FineQty),
                count(perfectDigit(N,DataNumber),PerfectQty).

fineDigit(N,DataNumber,Digit):-
                          nth1(Pos,N,Digit),
                          nth1(DataPos,DataNumber,Digit),
                          Pos \= DataPos. % If it's fine, it MUST be in a different place.
perfectDigit(N,DataNumber,Digit):-
                      nth1(Pos,N,Digit),
                      nth1(Pos,DataNumber,Digit). % If it's perfect, it MUST be on the same place.
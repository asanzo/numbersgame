:- module(numbersGame,
        [ winnerNumber/1,
          possible/1,
          fine/3,
          perfect/3,
          probability/2,
          probability/3
        ]).

:- use_module(accessories).

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
            forall(nth1(Pos,N,Dig),canBePlaced(Dig,Pos)),  % All digits canBePlaced in their position.
            not(discarded(N)).

%=============
% Auxiliares
%=============

validNumber(N):-
           rightSize(N),
           eachDigitIsValid(N),
           repeatedLess(N).

rightSize(N):- numberSize(S),length(N,S).

eachDigitIsValid([H]):- digit(H).
eachDigitIsValid([H|T]):- digit(H), eachDigitIsValid(T).

digit(D):- between(0,9,D).
position(P):- numberSize(S), between(1,S,P).

% Total of Just Fine + Perfect hits.
data(N,HitQty):-
              data(N,Perfect,Fine),
              HitQty is Perfect + Fine.

%==================================================================
% A Digit Can Be Placed in a Position
% (Probabilities)
%==================================================================

canBePlaced(Dig,Pos):- probability(Dig,Pos,1).       %If there is certainty, that's where the digit goes.
canBePlaced(Dig,Pos):-
                  not(probability(Dig,Pos,1)),   % If there isn't certainty,
                  not(probability(Dig,Pos,0)).   % it can be placed there provided that there is no way of getting 0 probability. 
                                                 % (all probabilities are greater than 0)
                  
% If no perfect digits whithin a data, just-fine digits DON'T go in those positions. 
%(0 probability of being placed there)
probability(D,Pos,0):-
                             data(N,0,_),  % 0 perfect digits 
                             nth1(Pos,N,D).

% If there are just perfect digits whithin a data, the digits can only go in that position
% That is to say, for every other position, each digit's probability will be 0.
probability(D,Pos,0):-
                             data(N,_,0), %  only perfect digits
                             nth1(OriginalPos,N,D),
                             position(Pos),
                             Pos \= OriginalPos. % other position than the original

% For probabilities that don't depend on the position of te digits but
% depend on the number of hits whithin a subset of digits:
% (probability is the ratio of how many hits in the set length)
probability(D,Pos,Prob):-
                          position(Pos),
                          setWithHits(Digits,HitQty),
                          member(D,Digits),
                          length(Digits,Qty),
                          Prob is HitQty / Qty.
                          
% First set with hits: all digits in a data, with its total of Just-Fine & Perfect hits.
setWithHits(N,HitQty):-  data(N,HitQty).
                             
% If From N1 to N2 a set of digits is replaced, and N1 differs from N2 in te same qty of hits, then
% the set of digits taken out are all hits, and the set of replacement digits has no hits. 
setWithHits(HitDigits,AllQty):-   % all are hits, so the Qty of hits is the length of the set.
                       digitsReplaced(HitDigits,_),
                       length(HitDigits,AllQty).

setWithHits(NoHitDigits,0):-  % 0 because none is a hit.
                       digitsReplaced(_,NoHitDigits).

%Si entre dos datos con d�gitos distintos, suman tantos aciertos como CantidadDeDigitos - AciertosQFaltan,
%esos que faltan se distribuyen entre los d�gitos que no aparecen en ningun numero.
% El caso m�s com�n es que si entre 1234 y 5678 hay 4 d�gitos que van (AciertosQFaltan=0), los dos restantes (0 y 9) seguro no van.
% Si con esos n�meros van s�lo 3 d�gitos (AciertosQFaltan=1) de los dos restantes (0 y 9) va s�lo 1.
setWithHits(Restantes,AciertosQFaltan):-
                            data(N1,HitQty1),
                            data(N2,HitQty2),
                            forall(member(M,N1),not(member(M,N2))),  %Todos los d�gitos son distintos.
                            HitQty is HitQty1 + HitQty2,
                            numberSize(NS),
                            AciertosQFaltan is NS - HitQty,
                            findall(R,(digit(R),not(member(R,N1)),not(member(R,N2))),Restantes).
                            
digitsReplaced(ReplacedDigits,ReplacementDigits):-
                             data(N1,HitQty1),
                             data(N2,HitQty2),
                             HitDiference is HitQty1 - HitQty2,
                             findall(Dig,(member(Dig,N1),not(member(Dig,N2))),ReplacedDigits),
                             length(ReplacedDigits,HitDiference),
                             findall(Dig,(member(Dig,N2),not(member(Dig,N1))),ReplacementDigits).

%=============
% Descarte de N�meros Posibles
% el predicado descartado define si un conjunto de n�meros probables separadamente
% pueden formar parte del resultado en forma conjunta.
%=============

%Deben respetar todos los datos.
%quedan descartados si no respetan alguno de los datos.
discarded(N):-
               data(NDato,_),not(respeta(N,NDato)).
respeta(N,NDato):-
                data(NDato,CantBien,CantReg),
                count(fine(N,NDato),CantReg),
                count(perfect(N,NDato),CantBien).

fine(N,NDato,Digit):-
                          nth1(Pos,N,Digit),
                          nth1(PosDato,NDato,Digit),
                          Pos \= PosDato.
perfect(N,NDato,Digit):-
                      nth1(Pos,N,Digit),
                      nth1(Pos,NDato,Digit).

%==================================================================
% Elecci�n del mejor posible
%==================================================================
masProbable(N):-
                  maximum(probability(_),PMax),
                  probability(N,PMax).

%Supongo que la probabilidad de un n�mero es la suma de las m�ximas probabilidades de sus d�gitos en sus posiciones
probability(N,Prob):-
                            possible(N),
                            findall(DigProb, (nth1(Pos,N,Dig),greatestProbability(Dig,Pos,DigProb)), Ps),
                            sumlist(Ps,Prob).

greatestProbability(Dig,Pos,Prob):-
                                 firstResult( maximum(probability(Dig,Pos)), Prob ).
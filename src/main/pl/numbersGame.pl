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

%Hay ciertas probabilidades que son las mismas para un conjunto de números, sin saber en qué posición particular:
probability(D,Pos,Prob):-
                          position(Pos),
                          conjuntoConAciertos(Digits,HitQty),
                          member(D,Digits),
                          length(Digits,Cant),
                          Prob is HitQty / Cant.
                          
% La primera manera de saber un conjunto de números y cuántos de ellos van, es directa:
conjuntoConAciertos(N,HitQty):-  data(N,HitQty).
                             
%Si de N1 a N2 se sacaron dígitos y hay N menos de puntaje, todos esos dígitos iban y los que se pusieron no.
conjuntoConAciertos(DigQueVan,CantTodos):-
                       diferenciaDeDigitos(DigQueVan,_),
                       length(DigQueVan,CantTodos).

conjuntoConAciertos(DigQueNoVan,0):-
                       diferenciaDeDigitos(_,DigQueNoVan).

diferenciaDeDigitos(DigQueVan,DigQueNo):-
                             data(N1,HitQty1),
                             data(N2,HitQty2),
                             CantQueDifieren is HitQty1 - HitQty2,
                             findall(Dig,(member(Dig,N1),not(member(Dig,N2))),DigQueVan),
                             length(DigQueVan,CantQueDifieren),
                             findall(Dig,(member(Dig,N2),not(member(Dig,N1))),DigQueNo).

%Si entre dos datos con dígitos distintos, suman tantos aciertos como CantidadDeDigitos - AciertosQFaltan,
%esos que faltan se distribuyen entre los dígitos que no aparecen en ningun numero.
% El caso más común es que si entre 1234 y 5678 hay 4 dígitos que van (AciertosQFaltan=0), los dos restantes (0 y 9) seguro no van.
% Si con esos números van sólo 3 dígitos (AciertosQFaltan=1) de los dos restantes (0 y 9) va sólo 1.
conjuntoConAciertos(Restantes,AciertosQFaltan):-
                            data(N1,HitQty1),
                            data(N2,HitQty2),
                            forall(member(M,N1),not(member(M,N2))),  %Todos los dígitos son distintos.
                            HitQty is HitQty1 + HitQty2,
                            numberSize(NS),
                            AciertosQFaltan is NS - HitQty,
                            findall(R,(digit(R),not(member(R,N1)),not(member(R,N2))),Restantes).

%=============
% Descarte de Números Posibles
% el predicado descartado define si un conjunto de números probables separadamente
% pueden formar parte del resultado en forma conjunta.
%=============

% Un número queda descartado si:
% contiene los dígitos de un conjunto con aciertos,
% y la cantidad de aciertos es inferior al tamaño del conjunto.
% Es decir, si no todos los numeros del conjunto van, el numero se descarta.
% descartado(N):-
%                           conjuntoConAciertos(Digitos,CantAciertos),
%                           contiene(N,Digitos),
%                           length(Digitos,Cant),
%                           CantAciertos < Cant.

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
% Elección del mejor posible
%==================================================================
masProbable(N):-
                  maximum(probability(_),PMax),
                  probability(N,PMax).

%Supongo que la probabilidad de un número es la suma de las máximas probabilidades de sus dígitos en sus posiciones
probability(N,Prob):-
                            possible(N),
                            findall(DigProb, (nth1(Pos,N,Dig),greatestProbability(Dig,Pos,DigProb)), Ps),
                            sumlist(Ps,Prob).

greatestProbability(Dig,Pos,Prob):-
                                 firstResult( maximum(probability(Dig,Pos)), Prob ).
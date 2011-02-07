:- module(numbersGame,
        [ winnerNumber/1,
          possible/1,
          regular/3,
          bien/3,
          probabilidad/2,
          probabilidad/3
        ]).

:- use_module(accessories).

%==================================================================
% Datos
%==================================================================
numberSize(4).
% data(Number,Perfect Qty, Good Qty).
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
           tamanioCorrecto(N),
           cadaDigitoEsValido(N),
           sinRepetidos(N).

tamanioCorrecto(N):- numberSize(S),length(N,S).

cadaDigitoEsValido([H]):- digito(H).
cadaDigitoEsValido([H|T]):- digito(H), cadaDigitoEsValido(T).

digito(D):- between(0,9,D).
posicion(P):- numberSize(S), between(1,S,P).

cantDigitos(CD):- count(digito,CD).

data(N,CantAciertos):-
              data(N,B,R),
              CantAciertos is B + R.

%==================================================================
% Puede ir un dígito en cierta posición
% (Probabilidades)
%==================================================================

canBePlaced(Dig,Pos):- probabilidad(Dig,Pos,1).       %Si de alguna manera hay certeza, ese dígito va ahí.
canBePlaced(Dig,Pos):-
                  not(probabilidad(Dig,Pos,1)),   %Si no hay certeza,
                  not(probabilidad(Dig,Pos,0)).   %puede ir siempre y cuando no exista una chance de que dé 0.
                  
%Si en cierto dato no hay dígitos bien, significa que los regulares que haya no van en esa posición.
probabilidad(D,Pos,0):-
                             data(N,0,_),
                             nth1(Pos,N,D).

%Si en cierto dato no hay dígitos regulares, significa que la probabilidad individual de cada dígito es 0 para cualquier otra posición.
probabilidad(D,Pos,0):-
                             data(N,_,0),
                             nth1(PosOriginal,N,D),
                             posicion(Pos),
                             Pos \= PosOriginal.

%Hay ciertas probabilidades que son las mismas para un conjunto de números, sin saber en qué posición particular:
probabilidad(D,Pos,Prob):-
                          posicion(Pos),
                          conjuntoConAciertos(Digitos,CantAciertos),
                          member(D,Digitos),
                          length(Digitos,Cant),
                          Prob is CantAciertos / Cant.
                          
% La primera manera de saber un conjunto de números y cuántos de ellos van, es directa:
conjuntoConAciertos(N,CantAciertos):-  data(N,CantAciertos).
                             
%Si de N1 a N2 se sacaron dígitos y hay N menos de puntaje, todos esos dígitos iban y los que se pusieron no.
conjuntoConAciertos(DigQueVan,CantTodos):-
                       diferenciaDeDigitos(DigQueVan,_),
                       length(DigQueVan,CantTodos).

conjuntoConAciertos(DigQueNoVan,0):-
                       diferenciaDeDigitos(_,DigQueNoVan).

diferenciaDeDigitos(DigQueVan,DigQueNo):-
                             data(N1,CantAciertos1),
                             data(N2,CantAciertos2),
                             CantQueDifieren is CantAciertos1 - CantAciertos2,
                             findall(Dig,(member(Dig,N1),not(member(Dig,N2))),DigQueVan),
                             length(DigQueVan,CantQueDifieren),
                             findall(Dig,(member(Dig,N2),not(member(Dig,N1))),DigQueNo).

%Si entre dos datos con dígitos distintos, suman tantos aciertos como CantidadDeDigitos - AciertosQFaltan,
%esos que faltan se distribuyen entre los dígitos que no aparecen en ningun numero.
% El caso más común es que si entre 1234 y 5678 hay 4 dígitos que van (AciertosQFaltan=0), los dos restantes (0 y 9) seguro no van.
% Si con esos números van sólo 3 dígitos (AciertosQFaltan=1) de los dos restantes (0 y 9) va sólo 1.
conjuntoConAciertos(Restantes,AciertosQFaltan):-
                            data(N1,CantAciertos1),
                            data(N2,CantAciertos2),
                            forall(member(M,N1),not(member(M,N2))),  %Todos los dígitos son distintos.
                            CantAciertos is CantAciertos1 + CantAciertos2,
                            numberSize(NS),
                            AciertosQFaltan is NS - CantAciertos,
                            findall(R,(digito(R),not(member(R,N1)),not(member(R,N2))),Restantes).

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
                count(regular(N,NDato),CantReg),
                count(bien(N,NDato),CantBien).

regular(N,NDato,Digito):-
                          nth1(Pos,N,Digito),
                          nth1(PosDato,NDato,Digito),
                          Pos \= PosDato.
bien(N,NDato,Digito):-
                      nth1(Pos,N,Digito),
                      nth1(Pos,NDato,Digito).

%==================================================================
% Elección del mejor posible
%==================================================================
masProbable(N):-
                  maximum(probabilidad(_),PMax),
                  probabilidad(N,PMax).

%Supongo que la probabilidad de un número es la suma de las máximas probabilidades de sus dígitos en sus posiciones
probabilidad(N,Prob):-
                            possible(N),
                            findall(ProbDig, (nth1(Pos,N,Dig),mayorProbabilidad(Dig,Pos,ProbDig)), Ps),
                            sumlist(Ps,Prob).

mayorProbabilidad(Dig,Pos,Prob):-
                                 firstResult( maximum(probabilidad(Dig,Pos)), Prob ).
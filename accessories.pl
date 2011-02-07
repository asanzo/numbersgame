:- module(accessories,
        [ justOne/1,
          firstResult/2,
          count/2,
          maximum/2,
          sinRepetidos/1,
          sinRepetidos/2,
          contiene/2
        ]).
          

justOne(Pred):- count(Pred,1).

firstResult(Pred,X):- findall(R,call(Pred,R),L), L=[X|_].

count(Pred,Qty):- findall(X,call(Pred,X),L), length(L,Qty).

maximum(Pred,Max):- call(Pred,Max),
                    forall(call(Pred,Value),Max >= Value).

sinRepetidos(L):- forall(member(X,L),noEstaEnElResto(X,L)).
noEstaEnElResto(X,L):- select(X,L,Resto), not(member(X,Resto)).

sinRepetidos([X|Xs],[X|Xr]):- not(member(X,Xs)),sinRepetidos(Xs,Xr).
sinRepetidos([X|Xs], Xr):- member(X,Xs),sinRepetidos(Xs,Xr).
sinRepetidos([],[]).

contiene(L,SubL):- select(_,L,SubL1), permutation(SubL1,SubL).
contiene(L,SubL):- select(_,L,SubL1), contiene(SubL1,SubL).
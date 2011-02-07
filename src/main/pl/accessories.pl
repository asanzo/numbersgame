:- module(accessories,
        [ justOne/1,
          firstResult/2,
          count/2,
          maximum/2,
          repeatedLess/1,
          repeatedLess/2,
          contains/2
        ]).
          

justOne(Pred):- count(Pred,1).

firstResult(Pred,X):- findall(R,call(Pred,R),L), L=[X|_].

count(Pred,Qty):- findall(X,call(Pred,X),L), length(L,Qty).

maximum(Pred,Max):- call(Pred,Max),
                    forall(call(Pred,Value),Max >= Value).

repeatedLess(L):- forall(member(X,L),noEstaEnElResto(X,L)).
noEstaEnElResto(X,L):- select(X,L,Resto), not(member(X,Resto)).

repeatedLess([X|Xs],[X|Xr]):- not(member(X,Xs)),repeatedLess(Xs,Xr).
repeatedLess([X|Xs], Xr):- member(X,Xs),repeatedLess(Xs,Xr).
repeatedLess([],[]).

contains(L,SubL):- select(_,L,SubL1), permutation(SubL1,SubL).
contains(L,SubL):- select(_,L,SubL1), contains(SubL1,SubL).
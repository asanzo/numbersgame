:- module(common,
        [ validNumber/1,
          digit/1,
          position/1,
          data/2
        ]).

:-use_module(accessories).
:-use_module(numbersGame).

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
data(N,HowManyAreHits):-
              data(N,Perfect,Fine),
              HowManyAreHits is Perfect + Fine.
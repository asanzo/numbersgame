:- module(numbersGame,
        [ winnerNumber/1,
          possible/1,
          fineDigit/3,
          perfectDigit/3,
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
            not(discarded(N)).

%=============
% Auxiliars
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
data(N,HowManyAreHits):-
              data(N,Perfect,Fine),
              HowManyAreHits is Perfect + Fine.
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

%==================================================================
% Best Possible Number - REVIEW
%==================================================================
bestPossibleNumber(N):-
                  maximum(probability(_),PMax),
                  probability(N,PMax).

% Suppose -wrongly: TODO- that the probability of a number is the sum of the max probability of each digit in its position.
probability(N,Prob):-
                            possible(N),
                            findall(DigProb, (nth1(Pos,N,Dig),greatestProbability(Dig,Pos,DigProb)), Ps),
                            sumlist(Ps,Prob).

greatestProbability(Dig,Pos,Prob):-
                                 firstResult( maximum(probability(Dig,Pos)), Prob ).

%==================================================================
% Probabilities for each digit
%==================================================================

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
                          setWithHits(Digits,HowManyAreHits),
                          member(D,Digits),
                          length(Digits,Qty),
                          Prob is HowManyAreHits / Qty.

% First set with hits: all digits in a data, with its total of Just-Fine & Perfect hits.
setWithHits(N,HowManyAreHits):-  data(N,HowManyAreHits).

% If From N1 to N2 a set of digits is replaced, and N1 differs from N2 in te same qty of hits, then
% the set of digits taken out are all hits, and the set of replacement digits has no hits.
setWithHits(HitDigits,AllQty):-   % all are hits, so the Qty of hits is the length of the set.
                       digitsReplaced(HitDigits,_),
                       length(HitDigits,AllQty).

setWithHits(NoHitDigits,0):-  % 0 because none is a hit.
                       digitsReplaced(_,NoHitDigits).

% Now we're contemplating "non-mentioned" digits: we're going to compare two
% data numbers and draw conclusions about the digits that don't appear in neither of them,
% but just when those data numbers have completely different digits.
% If that's the case, the sum of their hits + a MissingHitsQty is the length of a valid number,
% and the missing digits will have that MissingHitsQty.
% A common case arises between 1234 and 5678: if there are 4 hits between them (MissingHitsQty=0), remaining digits (0 & 9) are not hits.
% if there are 3 hits between them (MissingHitsQty=1) it means that one of the remaining digits (0 y 9) is a hit.
setWithHits(RemainingDigits,MissingHitsQty):-
                            data(N1,HitQty1),
                            data(N2,HitQty2),
                            forall(member(M,N1),not(member(M,N2))),  %Every digit is different.
                            HitSum is HitQty1 + HitQty2,
                            numberSize(NumberSize),
                            MissingHitsQty is NumberSize - HitSum,
                            findall(R,(digit(R),not(member(R,N1)),not(member(R,N2))),RemainingDigits).

digitsReplaced(ReplacedDigits,ReplacementDigits):-
                             data(N1,HitQty1),
                             data(N2,HitQty2),
                             HitDiference is HitQty1 - HitQty2,
                             findall(Dig,(member(Dig,N1),not(member(Dig,N2))),ReplacedDigits),
                             length(ReplacedDigits,HitDiference),
                             findall(Dig,(member(Dig,N2),not(member(Dig,N1))),ReplacementDigits).
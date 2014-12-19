% :- op(500,xfy, <==).

% prove(true,true).
% 
% prove((Goal1,Goal2), (Proof1, Proof2)) :-
%   prove(Goal1, Proof1),
%   prove(Goal2 , Proof2).
% 
% prove(Goal, Goal <== Proof) :-
%   clause(Goal,Body),
%   prove(Body,Proof).



prove(true, true).

prove(\+G,(\+G :- true)) :-
  \+exists_proof(G).

prove((G,Gs), ((G :- T1), T2)) :-
    G\==true,
    \+builtin(G),
    \+negated(G),
    clause(G, B),
    prove(B, T1),
    prove(Gs, T2).
    
prove(G,(G :- T)) :-
  G\==true,
  \+builtin(G),
  \+negated(G),
  clause(G,B),
  B \== call(G),
  prove(B,T).
  
negated(G) :- clause(G,\+call(G1)), !.
exists_proof(G) :- prove(G,T), !.
builtin(rdf(X,Y,Z)) :- !.
builtin(between(X,Y,Z)):- !.

% p(X) :- q(X), r(X), \+s(X).
% p(X) :- t(X).
% 
% q(X) :- t(X).
% 
% t(a).
% %q(a).
% %q(b).
% r(a).
% r(b).
% %s(a).
% s(b).


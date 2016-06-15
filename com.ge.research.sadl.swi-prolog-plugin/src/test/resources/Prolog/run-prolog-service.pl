:- style_check(-singleton).
:- consult('standard-declarations.pl').


%%%%%%%%%%%%%%%
% start service
%%%%%%%%%%%%%%%
:- consult('prolog-service.pl').
:- query:port_number(X), server(X), !.




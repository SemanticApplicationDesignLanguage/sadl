% loading semantic web libraries
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- rdf_load(library(semweb/rdfs)).

% defining properties of predicates
% properties of holds and derive are fixed as below
:- dynamic holds/5.
:- dynamic holds/4.
:- dynamic holds/3.
:- dynamic holds/2.
:- dynamic holds/1.

:- multifile holds/5.
:- multifile holds/4.
:- multifile holds/3.
:- multifile holds/2.
:- multifile holds/1.

:- discontiguous holds/5.
:- discontiguous holds/4.
:- discontiguous holds/3.
:- discontiguous holds/2.
:- discontiguous holds/1.

:- dynamic derive/5.
:- dynamic derive/4.
:- dynamic derive/3.
:- dynamic derive/2.
:- dynamic derive/1.

:- multifile derive/5.
:- multifile derive/4.
:- multifile derive/3.
:- multifile derive/2.
:- multifile derive/1.

:- discontiguous derive/5.
:- discontiguous derive/4.
:- discontiguous derive/3.
:- discontiguous derive/2.
:- discontiguous derive/1.

rdf(x, 'http://sadl.org/Shapes/Shapes#area', v2) :- rdf(x, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', 'http://sadl.org/Shapes/Rectangle#Rectangle') , rdf(x, 'http://sadl.org/Shapes/Rectangle#height', v0) , rdf(x, 'http://sadl.org/Shapes/Rectangle#width', v1) , *(v0,v1,v2).

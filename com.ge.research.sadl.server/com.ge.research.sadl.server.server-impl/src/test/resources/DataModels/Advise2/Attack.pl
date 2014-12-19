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

holds('http://www.illinois.edu/advise/System#PhysicalAccess','http://www.illinois.edu/advise/System#ofSystem',PT) :- holds('InstanceOf',PT,'http://www.illinois.edu/advise/System#PhysicalThing') , holds('Possible','http://www.illinois.edu/advise/Attack#GainPhysicalAccess','http://www.illinois.edu/advise/System#ofSystem',PT).
holds('Possible','http://www.illinois.edu/advise/Attack#GainPhysicalAccess','http://www.illinois.edu/advise/System#ofSystem',PT) :- holds('InstanceOf',PT,'http://www.illinois.edu/advise/System#PhysicalThing') , holds('http://www.illinois.edu/advise/System#LocationKnowledge','http://www.illinois.edu/advise/System#ofSystem',PT) , holds('InstanceOf',Skl,'http://www.illinois.edu/advise/Adversary#GettingIn') , holds('http://www.illinois.edu/advise/Adversary#Skilled',skilledIn,Skl).
holds('http://www.illinois.edu/advise/System#LogicalAccess','http://www.illinois.edu/advise/System#ofSystem',Sys) :- holds('System',SysC) , holds('InstanceOf',Sys,SysC) , holds('Possible','http://www.illinois.edu/advise/Attack#GainLogicalAccess','http://www.illinois.edu/advise/System#ofSystem',Sys).
holds('Possible','http://www.illinois.edu/advise/Attack#GainLogicalAccess','http://www.illinois.edu/advise/System#ofSystem',Sys) :- holds('System',SysC) , holds('InstanceOf',Sys,SysC) , holds('http://www.illinois.edu/advise/System#UIAccess','http://www.illinois.edu/advise/System#ofSystem',Sys) , holds('http://www.illinois.edu/advise/System#HasCredentials','http://www.illinois.edu/advise/System#ofSystem',Sys).
holds('http://www.illinois.edu/advise/System#UIAccess','http://www.illinois.edu/advise/System#ofSystem',Sys) :- holds('System',SysC) , holds('InstanceOf',Sys,SysC) , holds('Possible','http://www.illinois.edu/advise/Attack#GainLocalUIAccess','http://www.illinois.edu/advise/System#ofSystem',Sys).
holds('Possible','http://www.illinois.edu/advise/Attack#GainLocalUIAccess','http://www.illinois.edu/advise/System#ofSystem',Sys) :- holds('System',SysC) , holds('InstanceOf',Sys,SysC) , holds('http://www.illinois.edu/advise/Adversary#Skilled',skilledIn,'http://www.illinois.edu/advise/Adversary#Infiltration').
holds('http://www.illinois.edu/advise/System#HasCredentials','http://www.illinois.edu/advise/System#ofSystem',Sys) :- holds('System',SysC) , holds('InstanceOf',Sys,SysC) , holds('Possible','http://www.illinois.edu/advise/Attack#GainCredentials','http://www.illinois.edu/advise/System#ofSystem',Sys).
holds('Possible','http://www.illinois.edu/advise/Attack#GainCredentials','http://www.illinois.edu/advise/System#ofSystem',Sys) :- holds('System',SysC) , holds('InstanceOf',Sys,SysC).
holds('http://www.illinois.edu/advise/System#InformationAccess','http://www.illinois.edu/advise/System#ofSystem',Dt,'http://www.illinois.edu/advise/System#about',Th,Sys) :- holds('InstanceOf',Th,'http://www.illinois.edu/advise/System#Thing') , holds('System',SysC) , holds('InstanceOf',Sys,SysC) , holds('Controls',Sys,Th) , holds('InstanceOf',Dt,'http://www.illinois.edu/advise/System#Data') , holds('Possible','http://www.illinois.edu/advise/Attack#ReadData','http://www.illinois.edu/advise/System#ofSystem',Dt,'http://www.illinois.edu/advise/System#about',Th,Sys).
holds('Possible','http://www.illinois.edu/advise/Attack#ReadData','http://www.illinois.edu/advise/System#ofSystem',Dt,'http://www.illinois.edu/advise/System#about',Th,Sys) :- holds('InstanceOf',Th,'http://www.illinois.edu/advise/System#Thing') , holds('System',SysC) , holds('InstanceOf',Sys,SysC) , holds('Controls',Sys,Th) , holds('InstanceOf',Dt,'http://www.illinois.edu/advise/System#Data') , holds('http://www.illinois.edu/advise/System#LogicalAccess','http://www.illinois.edu/advise/System#ofSystem',Sys).
holds('System','http://www.illinois.edu/advise/System#Application').
holds('System','http://www.illinois.edu/advise/System#OperatingSystem').

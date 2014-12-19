%:- consult('standard-declarations.pl').

:- rdf_register_ns(adversary,'http://www.illinois.edu/advise/Adversary#').
:- rdf_register_ns(attack,'http://www.illinois.edu/advise/Attack#').
:- rdf_register_ns(firewall,'http://www.illinois.edu/advise/Firewall#').
:- rdf_register_ns(system,'http://www.illinois.edu/advise/System#').
:- rdf_register_ns(ex,'http://www.illinois.edu/advise/SmallExample#').



:- rdf_meta
   instanceOf(t,t).
:- rdf_meta
   activity(t).
:- rdf_meta
   prop(t).
:- rdf_meta
   object(t).
:- rdf_meta
   controls(t,t).





% static rules
max_class_depth(5).

activity(Ac) :- rdf(Ac,rdfs:subClassOf,system:'Activity').
activity(Ac) :- rdf(Ac,rdfs:subClassOf,X),
                rdf(X,rdfs:subClassOf,system:'Activity').
                
prop(Prop) :- rdf(Prop,rdf:type,owl:'ObjectProperty').
prop(Prop) :- rdf(Prop,rdf:type,owl:'DatatypeProperty').

object(Ob) :- instanceOf(Ob,C).

binstanceOf(I,C) :- rdf(I,rdf:type,C),
                   is_domain_concept(I),
                   is_domain_concept(C).
                   
% binstanceOf(I,C) :- rdf(I,rdf:type,X),
%                    rdf(X,owl:intersectionOf,L),
%                    is_member(L,C),
%                    is_domain_concept(I),
%                    is_domain_concept(C).

instanceOf(X,Y) :- binstanceOf(X,Y).
instanceOf(X,Y) :- subClassOf(Z,Y,D),
                   binstanceOf(X,Z).

subClassOf(X,Y,1) :- rdf(X,rdfs:subClassOf,Y),
                     is_domain_concept(X),
                     is_domain_concept(Y).
                     
subClassOf(X,Y,1) :- rdf(X,rdfs:subClassOf,X1),
                     rdf(X1,owl:intersectionOf,L),
                     is_member(L,Y),
                     is_domain_concept(X),
                     is_domain_concept(Y).
                     
subClassOf(X,Y,N) :- max_class_depth(Max),
                     between(1,Max,N),
                     N1 is N-1,
                     subClassOf(X,Z,N1),
                     subClassOf(Z,Y,1).

controls(Sys,D) :- rdf(Sys,system:function,X),
                   rdf(X,rdf:type,system:'Control'),
                   rdf(X,system:target,D),
                   is_domain_concept(Sys),
                   is_domain_concept(D).
                   
%instanceOf(system:pt1,system:'PhysicalThing').

is_domain_concept(E) :- sub_string(E,SI,Len,After,'http://www.illinois.edu/advise/').
is_member(L,E) :- rdf(L,rdf:first,E).
is_member(L,E) :- rdf(L,rdf:first,E1),
                  rdf(L,rdf:rest,L1),
                  rdf(L1,rdf:first,E).

                   
write_instances_pl :- open('instance.pl',write,Out,[]),
                   findall([I,C],instanceOf(I,C),ICL),
                   list_to_set(ICL,ICS),
                   findall([I1,C1],(member([I1,C1],ICS),
                                  atomic_list_concat(['holds(\'InstanceOf\',\'',I1,'\',\'',C1,'\').'],'',ToWrite),
                                  write(Out,ToWrite),nl(Out)),ICS1),
                   findall(Ac,activity(Ac),AL),
                   list_to_set(AL,AS),
                   findall(Ac1,(member(Ac1,AS),
                                  atomic_list_concat(['holds(\'Activity\',\'',Ac1,'\').'],'',ToWrite1),
                                  write(Out,ToWrite1),nl(Out)),AS1),
                   findall(P,prop(P),PL),
                   list_to_set(PL,PS),
                   findall(P1,(member(P1,PS),
                                  atomic_list_concat(['holds(\'Prop\',\'',P1,'\').'],'',ToWrite2),
                                  write(Out,ToWrite2),nl(Out)),PS1),
                   findall(O,object(O),OL),
                   list_to_set(OL,OS),
                   findall(O1,(member(O1,OS),
                                  atomic_list_concat(['holds(\'Object\',\'',O1,'\').'],'',ToWrite3),
                                  write(Out,ToWrite3),nl(Out)),OS1),
                   findall([S,D],controls(S,D),SDL),
                   list_to_set(SDL,SDS),
                   findall([S1,D1],(member([S1,D1],SDS),
                                  atomic_list_concat(['holds(\'Controls\',\'',S1,'\',\'',D1,'\').'],'',ToWrite4),
                                  write(Out,ToWrite4),nl(Out)),SDS1),
                   close(Out).
                   
assert_instances_pl :-
                   findall([I,C],instanceOf(I,C),ICL),
                   list_to_set(ICL,ICS),
                   findall([I1,C1],(member([I1,C1],ICS),
                                  atomic_list_concat(['holds(\'InstanceOf\',\'',I1,'\',\'',C1,'\').'],'',ToWrite),
                                  retractall(ToWrite), assertz(ToWrite)),ICS1),
                   findall(Ac,activity(Ac),AL),
                   list_to_set(AL,AS),
                   findall(Ac1,(member(Ac1,AS),
                                  atomic_list_concat(['holds(\'Activity\',\'',Ac1,'\').'],'',ToWrite1),
                                  retractall(ToWrite1), assertz(ToWrite1)),AS1),
                   findall(P,prop(P),PL),
                   list_to_set(PL,PS),
                   findall(P1,(member(P1,PS),
                                  atomic_list_concat(['holds(\'Prop\',\'',P1,'\').'],'',ToWrite2),
                                  retractall(ToWrite2), assertz(ToWrite2)),PS1),
                   findall(O,object(O),OL),
                   list_to_set(OL,OS),
                   findall(O1,(member(O1,OS),
                                  atomic_list_concat(['holds(\'Object\',\'',O1,'\').'],'',ToWrite3),
                                  retractall(ToWrite3), assertz(ToWrite3)),OS1),
                   findall([S,D],controls(S,D),SDL),
                   list_to_set(SDL,SDS),
                   findall([S1,D1],(member([S1,D1],SDS),
                                  atomic_list_concat(['holds(\'Controls\',\'',S1,'\',\'',D1,'\').'],'',ToWrite4),
                                  retractall(ToWrite4), assertz(ToWrite4)),SDS1).
                  
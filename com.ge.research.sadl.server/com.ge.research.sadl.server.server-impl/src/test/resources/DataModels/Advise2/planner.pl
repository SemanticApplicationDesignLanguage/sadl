%:- consult('onto-interface.pl').
%:- consult('proof.pl').

plan(X) :- write_instances_pl,
           %unload_file('C:/projects/Advise/advise/SemanticModels/Advise2.fixed/Temp/instance.pl'),
           consult('instance.pl'),
           %assert_instances_pl,
           prove(holds('Goal'),Proof),
           with_output_to(atom(ProofS),writeq(Proof)),
           extract_plan(ProofS,X).
           
extract_plan(Proof,X) :- findall(SI,sub_string(Proof,SI,L,A,'holds(\'Possible\','),SIL),
                         sort(SIL,SIO),nth1(1,SIO,F),
                         %print(SIO),
                         findall(SI1,(sub_string(Proof,SI1,L1,A1,':-'),SI1>F),SI1L),
                         %print(SI1L),
                         append(SIO,SI1L,SICL),
                         sort(SICL,SICO),
                         %print(SICO),
                         findall([AcS,AcE],(member(AcS,SIO),nth1(I,SICO,AcS),I1 is I+1,nth1(I1,SICO,AcE)),SEL),
                         %print(SEL),
                         findall(Action,(member([S,E],SEL),Le is E-S+1,
                                         sub_string(Proof,S,Le,A2,Act),
                                         format_action(Act,Action)),ACL),
                         reverse(ACL,ACR),
                         atomic_list_concat(ACR,'  -->  ',X1),
                         atomic_list_concat(ACR1,'\'',X1),
                         atomic_list_concat(ACR1,'',X).
                         
format_action(Act,Action) :- sub_string(Act,17,L,2,ActC),
                             atomic_list_concat(List,',',ActC),
                             atomic_list_concat(List,' ',Action1),
                             remove_ns(Action1,Action).
                             

remove_ns(X,Y) :- replace(X,'http://www.illinois.edu/advise/Attack#','',X1),
                  replace(X1,'http://www.illinois.edu/advise/System#','',X2),
                  replace(X2,'http://www.illinois.edu/advise/SmallExample#','',X3),
                  replace(X3,'http://www.illinois.edu/advise/Adversary#','',X4),
                  replace(X4,'http://www.illinois.edu/advise/Firewall#','',Y).
                  
replace(Src,Str,Rep,Tar) :- atomic_list_concat(SL,Str,Src),
                            atomic_list_concat(SL,Rep,Tar).
                         


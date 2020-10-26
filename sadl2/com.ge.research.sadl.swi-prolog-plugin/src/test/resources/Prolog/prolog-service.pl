:- module(query, [ server/1]).

:- use_module(library(http/httpd)).
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_mime_plugin)).
:- use_module(library(http/http_client)).
:- use_module(library(http/html_write)).
:- use_module(library(lists)).
:- use_module(library(http/js_write)).
:- use_module(library(sgml)).
:- use_module(library(http/http_session)).
:- use_module(library(thread_pool)).
:- use_module(library(broadcast)).
:- use_module(library(http/json)).
:- use_module(library(http/json_convert)).
:- use_module(library(http/http_json)).


:- thread_pool_create(compute, 3,
                      [ local(20000), global(100000), trail(50000),
                        backlog(5)
                      ]).


:- http_handler(root(reasoning_server), reasoning_server, [spawn(compute)]).
:- http_handler(root(result),result,[spawn(compute)]).
:- http_handler(root(predefinedResult),predefinedResult,[spawn(compute)]).
:- http_handler(root(image),image,[spawn(compute)]).

:- listen(http_session(end(SessionId, Peer)),
          end_session(SessionId)).
          

:- consult('prolog-service-config/prolog-service-config.pl').
          
end_session(SessionId) :- tmp_dir(Dir),
                          atomic_list_concat([Dir,'temp-',SessionId,'.pl'],'',File),
                          unload_file(File),
                          (exists_file(File) -> delete_file(File)).
                          
get_session_file(File) :- tmp_dir(Dir),
                          http_session_id(SessionId),
                          atomic_list_concat([Dir,'temp-',SessionId,'.pl'],'',File).
                          
get_session_file(SessionId,File) :- tmp_dir(Dir),
                                    atomic_list_concat([Dir,'temp-',SessionId,'.pl'],'',File).
                          
          
%:- http_set_session_options([path(reasoning_server)]).

server(Port) :-
        http_server(http_dispatch, [port(Port)]).


%response(Request) :-  thread_create(result(Request),Id,[detached(true)]).
% result(Request) :- write(user_output,'result request received\n'), (   memberchk(method(post), Request),
%             http_read_data(Request, Parts, [form_data(mime)]),
%             member(mime(Attributes, Data, []), Parts)
%         ->  % process file here; this demo just prints the info gathered
%             reply(Request,Data)
%         ;   throw(http_reply(bad_request(bad_data)))
%         ).
        
result(Request) :- write(user_output,'result request received\n'), (   (memberchk(method(post), Request),
            http_read_data(Request, Data, []), write(user_output,Data), write(user_output,'\n'))
            %member(mime(Attributes, Data, []), Parts)
        ->  % process file here; this demo just prints the info gathered
            (reply(Request,Data))
        ;   (throw(http_reply(bad_request(bad_data))))
        ).
        
predefinedResult(Request) :- write(user_output,'Predefined query invoked\n'), (   memberchk(method(post), Request),
            http_read_data(Request, Data, []), write(user_output,Data), getCorrespondingQuery(Data,Query)
            %member(mime(Attributes, Data, []), Parts)
        ->  % process file here; this demo just prints the info gathered
            reply(Request,Query)
        ;   throw(http_reply(bad_request(bad_data)))
        ).

handle_json_request(Request) :-
      %write(user_output,Request),
      write(user_output,'reading request\n'),
      http_read_json(Request, JSONIn),
      write(user_output,JSONIn),
      json_to_prolog(JSONIn, PrologIn),
%       <compute>(PrologIn, PrologOut),           % application body
      prolog_to_json(PrologIn, JSONOut),
      reply_json(JSONOut).
      
reasoning_server(Request) :-
  write(user_output,'reasoning server request received\n'),
%   writeq(user_output,Request),
  write(user_output,'Request written\n'),
  handle_json_request(Request).
                   
reply(Request,[query=Query]) :- http_session_id(SessionId),
                      get_session_file(SessionId,File),
                      open(File,write,Stream),
                      atomic_list_concat([':- module(\'',SessionId,'\',[]).'],'',Module),
                      write(Stream,Module),nl(Stream),
                      %term_to_atom(Data,DataT),
                      %sub_string(DataT,7,Len,1,Query),
                      write(user_output,'writing query \n'),
                      write(user_output,Query),
                      write(user_output,'\n'),
                      %sub_string(Query,1,Len1,1,QueryToWrite),
                       write(Stream,Query),
                       nl(Stream),
                       close(Stream),
                       consult(File),
                       %atomic_list_concat(['\'',SessionId,'\''],'',SessionIdStr),
                       findall(L,SessionId:qresult(L),LL),
                       %writeq(user_output,LL), write(user_output,'\n'),
                       %qresult(LL),
                       SessionId:targetVar(Var),
                       list_results(Request,Var,LL).
                       %close(LogStream).
                       
                       
:- multifile prolog:message//1.

prolog:message(bad_data) -->
        [ 'Please enter a query.'
        ].




%       Create a table of all available modules with their source-file

list_results(Request,Var,RL) :-
        reply_html_page(title('Query Results'),
                        [ h1('Query Results'),
                          table([class=table2],[ \header(Var)
                                | \results(RL)
                                ])
                        ]).


header(Var) -->
            html(tr([\head_elements(Var)])).

head_elements([]) --> [].
head_elements([H|T]) -->
      html(th(H)), head_elements(T).




results([]) --> [].
results([H|T]) -->
            %{write(user_output,'writing '),write(user_output,H),write(user_output,'\n')},
            html(tr([\result_elements(H)])),
            results(T).



result_elements([]) --> [].
result_elements([H|T]) -->
                   %{write(user_output,'writing '),write(user_output,H),write(user_output,'\n')},
                   html(td(H)),result_elements(T).
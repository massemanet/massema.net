%% -*- mode: erlang; erlang-indent-level: 2 -*-
%%% Created : 27 Jun 2012 by mats cronqvist <masse@klarna.com>

%% @doc
%% the massema.net server
%% @end

-module(mm).
-author('mats cronqvist').
-export([start/0
         ,test/0
         ,do/2]).

start() ->
  [egeoip:start() || not is_started(egeoip)],
  [inets:start() || not is_started(inets)],
  inets:start(httpd,conf()).

is_started(A) -> lists:member(A,[X||{X,_,_}<-application:which_applications()]).

logg(E) -> error_logger:error_report(E).

%% we rely on the convention that the error log lives in ".../<app>/<logfile>"
%% and static pages lives in priv_dir/static
conf() ->
  {ok,{file,ErrorLog}} = application:get_env(kernel,error_logger),
  LogDir = filename:dirname(ErrorLog),
  Root = code:lib_dir(filename:basename(LogDir)),
  [{port, 8080},
   {server_name,atom_to_list(?MODULE)},
   {server_root,LogDir},
   {document_root,filename:join([Root,priv,static])},
   {modules, [mod_alias,mod_fun,mod_get,mod_log]},
   {directory_index, ["index.html"]},
   {error_log,filename:join([LogDir,"errors.log"])},
   {handler_function,{?MODULE,do}},
   {mime_types,[{"html","text/html"},
                {"css","text/css"},
                {"ico","image/x-icon"},
                {"js","application/javascript"}]}].

%% called from the server
%% we can deliver the content in chunks by calling Act(Chunk).
%% the first chunk can be headers; [{Key,Val}]
%% if we don't want to handle the request, we do Act(defer)
%% if we crash, there will be a 404
do(Act,Req) ->
  case mustache_file(Req) of
    "" -> Act(defer);
    MF -> Act(mustache(MF,Req))
  end.

mustache_file(Req) ->
  try
    {Name,_} = proplists:get_value(real_name,Req(data)),
    ".html" = filename:extension(Name),
    MF = filename:rootname(Name,".html")++".mustache",
    true = filelib:is_regular(MF),
    MF
  catch
    _:_ -> ""
  end.

mustache(MF,Req) ->
  {ok,Bin} = file:read_file(MF),
  run(compile_mustache(binary_to_list(Bin)),Req).

%% compile the mustache file to internal form; a list, an alternating
%% sequence of html fragments (string) and funs.

compile_mustache(Str) ->
  try compile(parse(Str))
  catch throw:R -> logg(R),Str
  end.

%% parse the mustache file. Output is two lists; one list of html
%% "fragments" (strings), and one list of mustache "nuggets" (also strings).
%% the nuggets will be compiled to funs in the next stage.
%% 92 is the escape char, "\"
%% "\" is forbidden in mustache context. in html context, it escapes the next
%% character. E.g. "{\{" will become "{{", and "\\" will become "\".
-record(ms,{state=off,cfrag="",frags=[],cnugg="",nuggs=[]}).
parse(Str) ->
  try parse(Str,#ms{})
  catch throw:{R,Tail} -> logg(syntax_error(R,Tail,Str)), {[Str],[]}
  end.

parse([$},$}|R],#ms{state=on} = MS) -> parse(R,off(MS));
parse([92|R],   #ms{state=on})      -> throw({escape,R});
parse([${|R],   #ms{state=on})      -> throw({nesting,R});
parse([$}|R],   #ms{state=on})      -> throw({nesting,R});
parse([],       #ms{state=on})      -> throw({premature,""});
parse([C|R],    #ms{state=on} = MS) -> parse(R,MS#ms{cnugg=[C|MS#ms.cnugg]});
parse([$},$}|R],#ms{state=off})     -> throw({terminator,R});
parse([92,C|R], #ms{state=off}= MS) -> parse(R,MS#ms{cfrag=[C|MS#ms.cfrag]});
parse([${,${|R],#ms{state=off}= MS) -> parse(R,on(MS));
parse([C|R],    #ms{state=off}= MS) -> parse(R,MS#ms{cfrag=[C|MS#ms.cfrag]});
parse([],       #ms{state=off}= MS) -> done(MS).

on(MS) ->
  MS#ms{frags=[lists:reverse(MS#ms.cfrag)|MS#ms.frags],cfrag=[],state=on}.
off(MS) ->
  MS#ms{nuggs=[lists:reverse(MS#ms.cnugg)|MS#ms.nuggs],cnugg=[],state=off}.

done(#ms{frags=Frags,cfrag=CF,nuggs=Nuggs}) ->
  {lists:reverse([lists:reverse(CF)|Frags]),lists:reverse(Nuggs)}.

syntax_error(R,Tail,Str) ->
  {R,trim(Str,Tail)++"^"++trim(Tail)}.

trim(Str,Tail) -> lists:reverse(trim(lists:reverse(Str)--Tail)).
trim(Str) -> lists:sublist(Str,10).

%% compile nuggets to funs
compile({Fs,Ns}) ->
  compile(Fs,Ns).

compile([],[]) -> [];
compile([F1|Frags],Nuggets) ->
  [F1|cmp(Frags,Nuggets)].

cmp([],[]) -> "";
cmp([Frag|Frags],[Nugget|Nuggets]) ->
  [mcompile(Nugget),Frag|cmp(Frags,Nuggets)].

%% compile a mustache nugget to a fun
mcompile(Str) ->
  Types = types(),
  case string:tokens(Str,". ") of  %% this will not support escaped "."
    [] ->
      fun(_) -> "" end;
    [H|T] -> 
      F0 = case H of
             "''" -> fun(_) -> '' end;
             _ -> mdo(Types,H)
           end,
      Fs = [mdo(Types,I) || I <- T],
      fun(Ctxt) -> thread(F0(Ctxt),Fs) end
  end.

thread(Ctxt,[])     -> to_str(Ctxt);
thread(Ctxt,[F|Fs]) -> thread(F(Ctxt),Fs).

to_str(Term) ->
  case lists:flatten(io_lib:format("~p",[Term])) of
    "\""++Str -> lists:reverse(tl(lists:reverse(Str)));
    "'"++Str  -> lists:reverse(tl(lists:reverse(Str)));
    Str       -> Str
  end.

mdo(Types,I) ->
  case first(Types,I) of
    {M,F} -> wrap(M,F);
    X     -> wrap(X)
  end.

wrap(X) ->
  fun(Ctxt) ->
      case Ctxt of
        ""                       -> "";
        [{K,_}|_]                -> proplists:get_value(cast(X,K),Ctxt);
        [_|_]                    -> lists:nth(X,Ctxt);
        _ when is_tuple(Ctxt)    -> element(X,Ctxt);
        _ when is_function(Ctxt) -> Ctxt(X);
        _                        -> logg([{field,X},{ctxt,Ctxt}]),""
      end
  end.

wrap(ets,T) ->
  fun(Ctxt)->
      try element(2,hd(ets:lookup(T,Ctxt)))
      catch _:_ -> ""
      end
  end;
wrap(M,F) ->
  fun(Ctxt)->
      try
        case Ctxt of
          '' -> M:F();
          _  -> M:F(Ctxt)
        end
      catch _:_ -> ""
      end
  end.

cast(X,K) when is_list(K) -> atom_to_list(X);
cast(X,_) -> X.

first([T|Ts],I) ->
  try T(I)
  catch _:_ -> first(Ts,I)
  end.

types() ->
  [fun(I) -> [M,F] = string:tokens(I,":"),{list_to_atom(M),list_to_atom(F)}end,
   fun(I) -> list_to_integer(I)end,
   fun(I) -> list_to_atom(I)end].

%% run a mustache term.
%% returns a string.
run([],_) -> "";
run([F|R],Ctxt0) when is_function(F) -> F(Ctxt0)++run(R,Ctxt0);
run([F|R],Ctxt0) -> F++run(R,Ctxt0).

%% ad-hoc unit testing of the mustache compiler
test() ->
  FN = filename:join([code:priv_dir(massema.net),test,test.mustache]),
  {ok,FD} = file:open(FN,[read]),
  try test(FD,io:get_line(FD,''),io:get_line(FD,''))
  after file:close(FD)
  end.

test(_,eof,eof) -> ok;
test(FD,Line,A) ->
  R = run(compile_mustache(Line),[]),
  try R = A
  catch _:{badmatch,_} -> logg([{got,R},{expected,A}])
  end,
  test(FD,io:get_line(FD,''),io:get_line(FD,'')).


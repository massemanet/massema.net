%% -*- mode: erlang; erlang-indent-level: 2 -*-
%%% Created : 11 Mar 2013 by mats cronqvist <masse@klarna.com>

%% @doc
%% @end

-module('mustasch').
-author('mats cronqvist').
-export([file/2,
         run/2,
         test/0
        ]).

file(MustaschFile,Ctxt) ->
  run(generator(MustaschFile),Ctxt).

generator(MustaschFile) ->
  assert_ets(),
  {ok,Bin} = file:read_file(MustaschFile),
  Hash = erlang:md5(Bin),
  case ets:lookup(mustasch,MustaschFile) of
    [{_,Hash,Gen}] ->
      Gen;
    _ ->
      Gen = compile(Bin),
      ets:insert(mustasch,{MustaschFile,Hash,Gen}),
      Gen
  end.

%% compile the mustasch file to internal form; a list, an alternating
%% sequence of html fragments (string) and funs.

compile_mustasch(Bin) ->
  Str = binary_to_list(Bin),
  try compile(parse(Str))
  catch throw:R -> mm:logg(R),[Str]
  end.

%% parse the mustasch file. Output is two lists; one list of html
%% "fragments" (strings), and one list of mustasch "nuggets" (also strings).
%% the nuggets will be compiled to funs in the next stage.
%% 92 is the escape char, "\"
%% "\" is forbidden in mustasch context. in html context, it escapes the next
%% character. E.g. "{\{" will become "{{", and "\\" will become "\".
-record(ms,{state=off,cfrag="",frags=[],cnugg="",nuggs=[]}).
parse(Str) ->
  try parse(Str,#ms{})
  catch throw:{R,Tail} -> mm:logg(syntax_error(R,Tail,Str)), {[Str],[]}
  end.

parse(   #ms{state=on}, [${|R])    -> throw({nesting,R});
parse(   #ms{state=on}, [$}|R])    -> throw({nesting,R});
parse(   #ms{state=on}, [])        -> throw({premature,""});
parse(MS=#ms{state=on}, [$},$}|R]) -> parse(R,off(MS));
parse(MS=#ms{state=on}, [C|R])     -> parse(R,MS#ms{cnugg=[C|MS#ms.cnugg]});
parse(   #ms{state=off},[$},$}|R]) -> throw({terminator,R});
parse(MS=#ms{state=off},[92,C|R])  -> parse(R,MS#ms{cfrag=[C|MS#ms.cfrag]});
parse(MS=#ms{state=off},[${,${|R]) -> parse(R,on(MS));
parse(MS=#ms{state=off},[C|R])     -> parse(R,MS#ms{cfrag=[C|MS#ms.cfrag]});
parse(MS=#ms{state=off},[])        -> done(MS).

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

%% compile a mustasch nugget to a fun
mcompile(Str) ->
  case string:tokens(Str,". ") of  %% this will not support escaped "."
    [] ->
      fun(_) -> "" end;
    SubNuggets ->
      Types = types(),
      [F0|Fs] = [wrap(first(Types,SN)) || SN <- SubNuggets],
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

wrap({int,X}) ->
  fun(Ctxt) ->
      case Ctxt of
        [{_,_}|_]                -> proplists:get_value(X,Ctxt);
        [_|_]                    -> lists:nth(X,Ctxt);
        _ when is_tuple(Ctxt)    -> element(X,Ctxt);
        _                        -> mm:logg([{field,X},{ctxt,Ctxt}]),""
      end
  end;
wrap({func,{ets,T}}) ->
  fun(Ctxt)->
      try element(2,hd(ets:lookup(T,Ctxt)))
      catch _:_ -> mm:logg([{table,T},{ctxt,Ctxt}]),""
      end
  end;
wrap({func,{M,F}}) ->
  fun(Ctxt)->
      try
        case Ctxt of
          '' -> M:F();
          _  -> M:F(Ctxt)
        end
      catch _:R -> mm:logg([{mf,{M,F}},{ctxt,Ctxt},{reason,R}]),""
      end
  end;
wrap({null,null}) ->
  fun(_Ctxt) ->
      ''
  end;
wrap({string,S}) ->
  fun(Ctxt) ->
      case Ctxt of
        [{_,_}|_] -> proplists:get_value(S,Ctxt);
        _         -> S
      end
  end;
wrap({atom,A}) ->
  fun(Ctxt) ->
      case Ctxt of
        [{_,_}|_] -> proplists:get_value(A,Ctxt);
        _         -> A
      end
  end.

first([T|Ts],I) ->
  try T(I)
  catch _:_ -> first(Ts,I)
  end.

types() ->
  [fun(I) -> {null  ,list_to_null(I)} end,
   fun(I) -> {func  ,list_to_mf(I)} end,
   fun(I) -> {string,list_to_string(I)} end,
   fun(I) -> {int   ,list_to_integer(I)} end,
   fun(I)->  {atom  ,list_to_atom(I)} end].

list_to_null("''") ->
  null.

list_to_mf(I) ->
  [M,F]=string:tokens(I,":"),
  {list_to_atom(M),list_to_atom(F)}.

list_to_string(I) ->
  "\""++X = I,
  "\""++S = lists:reverse(X),
  lists:reverse(S).

%% run a mustasch term.
%% returns a string.
run([],_) -> "";
run([F|R],Ctxt0) when is_function(F) -> F(Ctxt0)++run(R,Ctxt0);
run([F|R],Ctxt0) -> F++run(R,Ctxt0).

%% ad-hoc unit testing of the mustasch compiler
test() ->
  FN = filename:join([code:priv_dir(massema.net),test,test.mustasch]),
  {ok,FD} = file:open(FN,[read]),
  try test(FD,io:get_line(FD,''),io:get_line(FD,''))
  after file:close(FD)
  end.

test(_,eof,eof) -> ok;
test(FD,Line,A) ->
  R = run(compile_mustasch(Line),[]),
  try R = A
  catch _:{badmatch,_} -> mm:logg([{got,R},{expected,A}])
  end,
  test(FD,io:get_line(FD,''),io:get_line(FD,'')).

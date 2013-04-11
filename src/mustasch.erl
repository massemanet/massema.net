%% -*- mode: erlang; erlang-indent-level: 2 -*-
%%% Created : 11 Mar 2013 by mats cronqvist <masse@klarna.com>

%% @doc
%% @end

-module('mustasch').
-author('mats cronqvist').
-export([is_file/1,
         file/2,
         run/2,
         test/0
        ]).

%% if the request is for X.html, and X.mustasch exists.
is_file(Name) ->
  try
    ".html" = filename:extension(Name),
    MF = filename:rootname(Name,".html")++".mustasch",
    true = filelib:is_regular(MF),
    MF
  catch
    _:_ -> no
  end.

file(MustaschFile,Ctxt) ->
  run(generator(MustaschFile),Ctxt).

generator(MustaschFile) ->
  assert_ets(),
  {ok,Bin} = file:read_file(MustaschFile),
  Hash = erlang:md5(Bin),
  case lookup_ets(MustaschFile) of
    [{_,Hash,Gen}] ->
      Gen;
    _ ->
      Gen = compile(Bin),
      insert_ets({MustaschFile,Hash,Gen}),
      Gen
  end.

%% compile the mustasch file to internal form; a list, an alternating
%% sequence of html fragments (string) and funs.

compile(Bin) ->
  Str = binary_to_list(Bin),
  try comp(parse(lex(Str)))
  catch throw:R -> mm:logg(R),[Str]
  end.

%% lexer
lex(Str) ->
  {ok,Toks,_} = mustasch_lexer:string(Str),
  Toks.

%% parser
parse(Toks) ->
  pp(n,Toks,[[]]).

pp(n,[{'.' ,_  }|Toks],A) -> pp(n,Toks,[["."|hd(A)]|tl(A)]);
pp(n,[{':' ,_  }|Toks],A) -> pp(n,Toks,[[":"|hd(A)]|tl(A)]);
pp(n,[{em  ,_,M}|Toks],A) -> pp(n,Toks,[[M|hd(A)]|tl(A)]);
pp(n,[{dq  ,_,S}|Toks],A) -> pp(n,Toks,[["\"",S,"\""|hd(A)]|tl(A)]);
pp(n,[{sq  ,_,S}|Toks],A) -> pp(n,Toks,[["'",S,"'"|hd(A)]|tl(A)]);
pp(n,[{uq  ,_,S}|Toks],A) -> pp(n,Toks,[[S|hd(A)]|tl(A)]);
pp(n,[{'{{',_  }|Toks],A) -> pp(m,Toks,[[],finalize_str(hd(A))|tl(A)]);
pp(m,[{'}}',_  }|Toks],A) -> pp(n,Toks,[[],finalize_nug(hd(A))|tl(A)]);
pp(m,[T         |Toks],A) -> pp(m,Toks,[[T|hd(A)]|tl(A)]);
pp(n,[               ],A) -> lists:reverse([finalize_str(hd(A))|tl(A)]).

finalize_str(S) ->
  lists:flatten(lists:reverse(S)).

finalize_nug(Toks) ->
  lists:reverse(Toks).

%% compile nuggets to funs
comp({Fs,Ns}) ->
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

%% ets helpers
assert_ets() ->
  case ets:info(mustasch,size) of
    undefined -> ets:new(mustasch,[public,named_table,ordered_set]);
    _ -> ok
  end.

lookup_ets(K) ->
  ets:lookup(mustasch,K).

insert_ets(T) ->
  ets:insert(mustasch,T).

%% ad-hoc unit testing of the mustasch compiler
test() ->
  [test(I) || I <- [lexer,parser]].

test(lexer) -> lex(tf());
test(parser)-> mustasch_parser:parse(lex(tf())).

tf() ->
  FN = filename:join([code:priv_dir(massema.net),test,test.mustasch]),
  {ok,Bin} = file:read_file(FN),
  binary_to_list(Bin).


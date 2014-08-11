%% -*- mode: erlang; erlang-indent-level: 2 -*-
%%% Created : 11 Mar 2013 by mats cronqvist <masse@klarna.com>

%% @doc
%% @end

-module('mustasch').
-author('mats cronqvist').
-export([is_file/1,
         file/2,
         test/0,test/1
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

%% compile the mustasch file to internal form; a list of fun/1
compile(Bin) ->
  Str = binary_to_list(Bin),
  try gen(parse(lex(Str)))
  catch throw:R -> mm:logg(R),[Str]
  end.

%% lexer
lex(Str) ->
  case split_at_mustasch(Str,[]) of
    {S,""} -> [{uq,99,lists:reverse(S)}];
    {S,M} ->
      {done,{ok,Toks,_},C} = mustasch_lexer:tokens([],M),
      [{uq,0,lists:reverse(S)},{'{{',0}|Toks]++lex(C)
  end.

split_at_mustasch(Str,B) ->
  case Str of
    "{{"++M -> {B,string:strip(M,left)};
    []      -> {B,""};
    [H|T]   -> split_at_mustasch(T,[H|B])
  end.

%% parser
parse(Toks) ->
  {ok,P} = mustasch_parser:parse(Toks),
  P.

%% generate mustasch code (a list of fun/1)
gen(Nuggs) ->
  [mk_fun(N) || N <- Nuggs].

mk_fun(N) when is_list(N) ->
  fun(_) -> N end;
mk_fun({[{},{M,F}|N]}) ->
  Fs = [wrap(SN) || SN <- N],
  fun(_) -> thread(M:F(),Fs) end;
mk_fun({N}) ->
  [F0|Fs] = [wrap(SN) || SN <- N],
  fun(Ctxt) -> thread(F0(Ctxt),Fs) end.

thread(Ctxt,[])     ->
  assert_string(Ctxt);
thread(Ctxt,[F|Fs]) ->
  try thread(F(Ctxt),Fs)
  catch _:X -> mm:logg(X),""
  end.

assert_string(X) ->
  case is_string(X) of
    true -> X;
    false-> lists:flatten(io_lib:fwrite("~p",[X]))
  end.

-define(is_c(X),9=:=X orelse 10=:=X orelse 13=:=X orelse (31<X andalso X<256)).
is_string([])                  -> true;
is_string([C|R]) when ?is_c(C) -> is_string(R);
is_string(_)                   -> false.

wrap(X) when is_integer(X) ->
  fun(Ctxt) ->
      case Ctxt of
        [{_,_}|_]             -> proplists:get_value(X,Ctxt);
        [_|_]                 -> lists:nth(X,Ctxt);
        _ when is_tuple(Ctxt) -> element(X,Ctxt);
        _                     -> throw([{field,X},{ctxt,Ctxt}])
      end
  end;
wrap({ets,T}) ->
  fun(Ctxt)->
      try element(2,hd(ets:lookup(T,Ctxt)))
      catch _:_ -> throw([{table,T},{ctxt,Ctxt}])
      end
  end;
wrap({}) ->
  fun(_) ->
      ""
  end;
wrap({M,F}) ->
  fun(Ctxt) ->
      try M:F(Ctxt)
      catch _:R -> throw([{mf,{M,F}},{ctxt,Ctxt},{reason,R}]),""
      end
  end;
wrap(S) when is_list(S) ->
  fun(Ctxt) ->
      case Ctxt of
        [{_,_}|_] -> proplists:get_value(S,Ctxt);
        _         -> S
      end
  end;
wrap(A) when is_atom(A) ->
  fun(Ctxt) ->
      case Ctxt of
        [{_,_}|_] -> proplists:get_value(A,Ctxt);
        _         -> A
      end
  end.

%% run a mustasch term.
%% returns a string.
run([],_) -> "";
run([F|R],Ctxt0) -> F(Ctxt0)++run(R,Ctxt0).

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
  [test(I) || I <- [lexer,parser,generator]].

test(lexer) -> lex(tf());

test(parser)-> parse(test(lexer));
test(generator)-> run(gen(test(parser)),[{init_data,[a,{1,[2],3},c]}]).

tf() ->
  FN = filename:join([code:priv_dir('massema.net'),test,'test.mustasch']),
  {ok,Bin} = file:read_file(FN),
  binary_to_list(Bin).

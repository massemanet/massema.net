%% -*- mode: erlang; erlang-indent-level: 2 -*-
%% @doc
%% @end

-module(mustasch_test).

-export([test/0,test/1]).

%% ad-hoc unit testing of the mustasch compiler
test() ->
  [test(I) || I <- [lex, parse, gen, run]].

test(lex) -> mustasch:lex(tf());
test(parse) -> mustasch:parse(test(lexer));
test(gen) -> mustasch:gen(test(parser));
test(run) -> mustasch:run(test(gen),[{init_data,[a,{1,[2],3},c]}]).

tf() ->
  {ok, Bin} = file:read_file("test.mustasch"),
  binary_to_list(Bin).

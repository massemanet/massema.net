%% -*- mode: erlang; erlang-indent-level: 2 -*-

%% @doc
%% the massema.net server
%% @end

-module(mm).
-author('mats cronqvist').

%% cowboy 1.1 handler
-export([init/3, handle/2, terminate/3]).

init({tcp, http}, Req, []) ->
  {ok, Req, #{}}.

handle(Req0, State0) ->
  {Req1, Info} = req_info(Req0),
  {State, Status, Headers, Body} = do_handle(Info, State0),
  {ok, Req2} = cowboy_req:reply(Status, Headers, Body, Req1),
  {ok, Req2, State}.

terminate(_Reason, _Req, _State) ->
  ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
req_info(Req0) ->
  {Method, Req1} = cowboy_req:method(Req0),
  {Path, Req2} = cowboy_req:path_info(Req1),
  {QS, Req3} = cowboy_req:qs_vals(Req2),
  {Req3, #{method => Method, path => Path, qs => QS}}.

do_handle(#{method := <<"GET">>, path := [<<"tick">>], qs := []}, State) ->
  T = round(1000-element(3, erlang:timestamp())/1000),
  receive
  after T -> ok
  end,
  {{Y,Mo,D},{H,Mi,S}} = erlang:universaltime(),
  {State,
   200,
   [{<<"content-type">>, <<"text/plain">>}],
   io_lib:fwrite("~w-~.2.0w-~.2.0w ~.2.0w:~.2.0w:~.2.0w",[Y,Mo,D,H,Mi,S])};
do_handle(#{method := Meth, path := Path, qs := QS}, State) ->
  {State,
   200,
   [{<<"content-type">>, <<"text/plain">>}],
   [Meth, $:, lists:join($,,Path), $:, [[${, K, $,, V, $}] || {K, V} <- QS]]}.

%% do(Act,Req) ->
%%   {Name,_} = proplists:get_value(real_name,Req(data)),
%%   case {is_tick(Req),mustasch:is_file(Name)} of
%%     {no,no} -> Act(defer);
%%     {yes,no}-> Act(ticker());
%%     {no,MF} -> Act(mustasch:file(MF,Req(all)))
%%   end.

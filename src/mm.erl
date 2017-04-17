%% -*- mode: erlang; erlang-indent-level: 2 -*-

%% @doc
%% the massema.net server
%% @end

-module(mm).
-author('mats cronqvist').

%% cowboy 1.1 handler
-export([init/3, handle/2, terminate/3]).

init({tcp, http}, Req, State) ->
  {ok, Req, State}.

handle(Req0, State0) ->
  {Req1, Info} = req_info(Req0),
  {Status, State, Headers, Body} = do_handle(Info, State0),
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
  ship(200, State, text, synched_ts());
do_handle(#{method := <<"GET">>, path := Path, qs := []}, State) ->
  case can_ship(Path, State) of
    {ContentType, Body} -> ship(200, State, ContentType, Body);
    undefined -> ship(404, State, html, a404("GET", Path, []))
  end;
do_handle(#{method := Meth, path := Path, qs := QS}, State) ->
  ship(404, State, html, a404(Meth, Path, QS)).

can_ship(Path, #{root := Root}) ->
  try
    Safer = filename:safe_relative_path(filename:join(Path)),
    {ok, F} = file:read_file(filename:join(Root, Safer)),
    {html, F}
  catch
    _:_ -> undefined
  end.

a404(Meth, Path, QS) ->
  ["<h3>I'm confused; this doesn't make sense to me:</h3><p>",
   Meth, $:, lists:join($,,Path), $:, [[${, K, $,, V, $}] || {K, V} <- QS],
   "<p>I'm just a raspberry pi :<"].

ship(Status, State, ContentType, Body) ->
  {Status, State, content_type(ContentType), Body}.

content_type(text) -> [{<<"content-type">>, <<"text/plain">>}];
content_type(html) -> [{<<"content-type">>, <<"text/html">>}].

synched_ts() ->
  {{Y, Mo, D}, {H, Mi, S}} = synch(),
  io_lib:fwrite("~w-~.2.0w-~.2.0w ~.2.0w:~.2.0w:~.2.0w",[Y, Mo, D, H, Mi, S]).

synch() ->
  T = round(1000-element(3, erlang:timestamp())/1000),
  receive
  after T -> erlang:universaltime()
  end.

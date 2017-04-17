%% -*- mode: erlang; erlang-indent-level: 2 -*-
%% @doc
%% @end

-module(mm_app).

-export([start/2, stop/1]).

start(normal, placeholder) ->
  mm_sup:start_link().

stop(_) ->
  ok.

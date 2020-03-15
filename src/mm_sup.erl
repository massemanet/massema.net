%% -*- mode: erlang; erlang-indent-level: 2 -*-
%% @doc
%% @end

-module(mm_sup).

-behaviour(supervisor).

%%% API
-export([start_link/0]).

%%% supervisor behaviour callback
-export([init/1]).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, placeholder).

init(placeholder) ->
  setup_cowboy(),
  {ok, {{one_for_one, 10, 60}, []}}.

setup_cowboy() ->
  Root = filename:join([code:priv_dir('massema.net'),static]),
  Dispatch = cowboy_router:compile([{'_', [{"/[...]", mm, #{root=>Root}}]}]),
  {ok, _} = cowboy:start_http(mm_listener,
                              100,
                              [{port, 8080}],
                              [{env, [{dispatch, Dispatch}]}]).

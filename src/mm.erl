%% -*- mode: erlang; erlang-indent-level: 2 -*-
%%% Created : 27 Jun 2012 by mats cronqvist <masse@klarna.com>

%% @doc
%% the massema.net server
%% @end

-module(mm).
-author('mats cronqvist').
-export([ start/0
         ,do/3]).

start() ->
  inets:stop(),
  inets:start(),
  inets:start(httpd,conf()).

conf() ->
  Root = filename:dirname(filename:dirname(code:which(?MODULE))),
  [{port, 8080},
   {server_name,atom_to_list(?MODULE)},
   {server_root,ensure(filename:join([Root,server]))},
   {document_root,ensure(filename:join([Root,static]))},
   {modules, [mod_alias,mod_esi,mod_get,mod_log]},
   {directory_index, ["index.html"]},
   {error_log,filename:join([Root,server,"errors.log"])},
   {erl_script_alias, {"/erl", [?MODULE]}},
   {erl_script_nocache,true},
   {mime_types,[{"html","text/html"},
                {"css","text/css"},
                {"js","application/javascript"}]}].

ensure(X) ->
  filelib:ensure_dir(X++"/"),
  X.

%% called when the server sees /<module>/do[/?]*
%% we can deliver the content in chunks, as long as do/3 does not return
do(SessionID,Env,Input) ->
  mod_esi:deliver(SessionID,
                  ["Content-Type: text/html\r\n\r\n", 
                   "<html><title>I am ",
                   flat(node()),
                   "</title><body><h2>",
                   flat(?MODULE),
                   "</h2>Input:<tt>",
                   flat(Input),
                   "</tt>"]),
  mod_esi:deliver(SessionID,
                  ["<br>Env:",
                   flat(Env),
                   "</body></html>"]).

flat(X) ->
  lists:flatten(io_lib:fwrite("~p",[X])).

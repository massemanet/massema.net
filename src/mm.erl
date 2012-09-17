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
   {modules, [mod_alias,mod_esi,mod_get,mod_log]},
   {directory_index, ["index.html"]},
   {error_log,filename:join([LogDir,"errors.log"])},
   {erl_script_alias, {"/erl", [?MODULE]}},
   {erl_script_nocache,true},
   {mime_types,[{"html","text/html"},
                {"css","text/css"},
                {"js","application/javascript"}]}].

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

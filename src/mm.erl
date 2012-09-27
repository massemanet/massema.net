%% -*- mode: erlang; erlang-indent-level: 2 -*-
%%% Created : 27 Jun 2012 by mats cronqvist <masse@klarna.com>

%% @doc
%% the massema.net server
%% @end

-module(mm).
-author('mats cronqvist').
-export([ start/0
         ,dtl/2]).

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
   {modules, [mod_alias,mod_fun,mod_get,mod_log]},
   {directory_index, ["index.html"]},
   {error_log,filename:join([LogDir,"errors.log"])},
   {handler_function,{?MODULE,dtl}},
   {mime_types,[{"html","text/html"},
                {"css","text/css"},
                {"ico","image/x-icon"},
                {"js","application/javascript"}]}].

%% called from the server
%% we can deliver the content in chunks by sending {self(),Chunk} to P.
%% the first chunk can be headers; [{Key,Val}]
%% if we don't want to handle the request, we do exit(defer)
%% if we crash, there will be a 404
dtl(P,Request) ->
  case filename:extension(gt(request_uri,Request)) of
    ".html" -> 
      case file_exists(Request) of
        true -> exit(defer);
        false->
          P ! {self(),"<h1>h1</h1>"},
          P ! {self(),flat(Request)}
      end;
    _ -> exit(defer)
  end.

flat(X) ->
  lists:flatten(io_lib:fwrite("~p",[X])).

file_exists(Request) ->
  case gt(real_name,gt(data,Request)) of
    {Name,_} -> filelib:is_regular(Name);
    _ -> false
  end.

gt(Key,List) -> proplists:get_value(Key,List).

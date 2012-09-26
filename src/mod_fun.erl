-module(mod_fun).
%% the rather grandiosely named "ERLANG WEB SERVER CALLBACK API"
-export([do/1, load/2, store/2]).

-include_lib("inets/include/httpd.hrl").
-include_lib("inets/src/http_server/httpd_internal.hrl").
-include_lib("inets/src/inets_app/inets_internal.hrl").

-define(VMODULE,"FUN").

%% the configuration parameters
default(handler_timeout) -> 15000;
default(handler_function) -> {"",""}.

%% "ERLANG WEB SERVER API CALLBACK FUNCTIONS"
do(ModRec) ->
  case defer_response(ModRec) of
    false-> {proceed,safe_handle(ModRec)};
    true -> {proceed, ModRec#mod.data}
  end.

load("HandlerFunction " ++ HandlerFunction, []) ->
  try
    [Mod,Fun] = string:tokens(HandlerFunction," :"),
    {ok, [], {handler_function, {list_to_atom(Mod),list_to_atom(Fun)}}}
  catch _:_ ->
      {error, ?NICE(HandlerFunction ++ " is an invalid HandlerFunction")}
  end;
load("HandlerTimeout " ++ HandlerTimeout, []) ->
  try
    [TO] = string:tokens(HandlerTimeout," "),
    {ok, [], {handler_timeout, {list_to_integer(TO)}}}
  catch _:_ ->
      {error, ?NICE(HandlerTimeout ++ " is an invalid HandlerTimeout")}
  end.

store({handler_function, {M,F}} = Conf, _) when is_atom(M),is_atom(F)->
  {ok, Conf};
store({handler_integer, TO} = Conf, _) when is_integer(TO)->
  {ok, Conf};
store(Conf,_) ->
  {error,Conf}.

%% we guarantee that handle/1 succeeds or we generate a 404.
safe_handle(ModRec) ->
  try handle(ModRec)
  catch _:_ -> fourofour(ModRec)
  end.

%% true if some other mod_* has already handled the request
defer_response(#mod{data=Data}) ->
  (proplists:get_value(response,Data) == undefined) andalso
    (proplists:get_value(status,Data) == undefined).

%%%========================================================================   
%% since the handler fun can send many chunks, or not used chunked
%%  encoding at all,  we keep state while waiting.
-record(s,{state=init,
           chunks=[],
           headers=[],
           status=200,
           path="",
           length=0,
           chunked_send_p,
           timeout}).

%% should we do chunked sending
chunked_send_p(#mod{config_db=Db,http_version=HTTPV}) ->
  (HTTPV =/= "HTTP/1.1") orelse httpd_response:is_disable_chunked_send(Db).

%% we spawn into the handler fun, monitors it, and waits for data chunks.
handle(ModRec) ->
  Mod = lists:zip(record_info(fields,mod),tl(tuple_to_list(ModRec))),
  {M,F} = mod_get(ModRec,handler_function),
  Self = self(),
  S = #s{chunked_send_p=chunked_send_p(ModRec),
         timeout=mod_get(ModRec,handler_timeout)},
  loop(spawn_monitor(fun() -> M:F(Self,Mod) end),S,ModRec).

loop({Pid,Ref},S,ModRec) ->
  Timeout = S#s.timeout,
  receive 
    {Pid,Chunk}               -> loop({Pid,Ref},chunk(Chunk,S,ModRec),ModRec);
    {'DOWN',Ref,_,Pid,normal} -> twohundred(ModRec,S);
    {'DOWN',Ref,_,Pid,_}      -> fourofour(ModRec)
  after
    Timeout -> fiveofour(ModRec)
  end.

twohundred(ModRec,#s{state=has_header,headers=H,chunks=B,status=St}=S) ->
  send_unchunked(ModRec,H,St,B),
  [{response, {already_sent, 200, S#s.length}} | ModRec#mod.data];
twohundred(ModRec,#s{state=sent_headers}=S) ->
  send_final_chunk(ModRec),
  [{response, {already_sent, 200, S#s.length}} | ModRec#mod.data].

fourofour(#mod{request_uri=URI,data=Data}) ->
  [{status, {404, URI, "Not found"}} | Data].

fiveofour(ModRec) ->
  send_headers(false,ModRec,504,[]),
  httpd_socket:close(ModRec#mod.socket_type, ModRec#mod.socket),
%%  [{status,{504,ModRec#mod.request_uri,"Timeout"}} | ModRec#mod.data].
%% mod_esi send this... apparently because it already closed the socket.
  [{response, {already_sent, 200, 0}} | ModRec#mod.data].

mod_get(ModRec,Key) ->
  httpd_util:lookup(ModRec#mod.config_db,Key,default(Key)).

chunk(Chunk,S,ModRec) ->
  case S#s.state of
    init -> 
      case check_headers(S#s.chunks++Chunk) of
        {ok,Head,Body,Status} -> 
          case S#s.chunked_send_p of
            true -> 
              send_headers(true,ModRec,Status,Head),
              send_chunk(ModRec,Body),
              Len = S#s.length + length(Body),
              S#s{state=sent_headers,chunks=[],status=Status,length=Len};
            false->
              S#s{state=has_headers,headers=Head,chunks=Body,status=Status}
          end;
        {nok,C} ->
          S#s{chunks=C}
      end;
    sent_headers ->
      Len = S#s.length + length(Chunk),
      send_chunk(ModRec,Chunk),
      S#s{length=Len};
    has_headers -> 
      S#s{chunks=S#s.chunks++Chunk};
    _ -> 
      S
  end.

check_headers(Chunks) ->
  case httpd_esi:parse_headers(Chunks) of % ridiculously named split_at_eoh fun
    {[],_} -> 
      {nok,Chunks};
    {Head,Rest} -> 
      case httpd_esi:handle_headers(Head) of
        {ok, Headers, Status} -> {ok,Headers,Rest,Status}
      end
  end.

send_headers(false,ModRec,Status,Headers) ->
  send_headers(ModRec, Status, [{"connection","close"} | Headers]);
send_headers(true,ModRec,Status,Headers) ->
  send_headers(ModRec, Status, [{"transfer-encoding","chunked"} | Headers]).

send_headers(ModRec, Status, HTTPHeaders) ->
  ExtraHeaders = httpd_response:cache_headers(ModRec),
  httpd_response:send_header(ModRec, Status, ExtraHeaders ++ HTTPHeaders).

send_chunk(ModRec,Chunk) ->
  httpd_response:send_chunk(ModRec,Chunk,false).

send_final_chunk(ModRec) ->
  httpd_response:send_final_chunk(ModRec,false).

send_unchunked(ModRec,Headers,Status,Body) ->
  Len = integer_to_list(lists:flatlength(Body)),
  send_headers(false,ModRec,Status,[{"content-length",Len} | Headers]),
  httpd_response:send_body(ModRec,Status,Body).

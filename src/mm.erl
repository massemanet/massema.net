%% -*- mode: erlang; erlang-indent-level: 2 -*-

%% @doc
%% the massema.net server
%% @end

-module(mm).
-author('mats cronqvist').

%% API
-export([start/0]).

%% cowboy 1.1 handler
-export([init/3, handle/2, terminate/3]).

start() ->
  application:ensure_all_started(mm).

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
req_info(Req) ->
  Items = [cookies, headers, host, method,
          path_info, peer, port, qs_vals, url, version],
  lists:foldl(fun req_info/2, {Req, #{}}, Items).

req_info(Item, {Req0, Acc}) ->
  {Val, Req} = cowboy_req:Item(Req0),
  {Req, Acc#{Item => Val}}.

do_handle(Info, State) ->
  case Info of
    #{method := <<"GET">>, path_info := [<<"tick">>], qs_vals := []} ->
      ship(200, State, text, synched_ts());
    #{method := <<"GET">>, path_info := Path, qs_vals := []} ->
      case can_ship(Path, Info, State) of
        {ContentType, Body} -> ship(200, State, ContentType, Body);
        _ -> ship(404, State, html, a404("GET", Path, []))
      end;
    #{method := Meth, path_info := Path, qs_vals := QS} ->
      ship(404, State, html, a404(Meth, Path, QS))
  end.

can_ship(Path, Info, #{root := Root}) ->
  try
    Safer = safe_relative_path(Path),
    Filename = filename:join(Root, Safer),
    case file:read_file(Filename) of
      {ok, F} ->
        case extension(Safer) of
          "html" -> {html, mustasch:run(F, Info)};
          "md"   -> {html, markdown_convert(Filename, F)};
          "txt"  -> {text, F};
          "js"   -> {js, F};
          "css"  -> {css, F};
          "jpg"  -> {jpg, F};
          "png"  -> {png, F};
          "ico"  -> {ico, F};
          E      -> error_logger:error_report([{error, E}, {path, Path}])
        end;
      {error, eisdir} ->
        {ok, Fs} =  file:list_dir(Filename),
        Files = lists:sort(lists:filter(fun only_md/1, Fs)),
        {html, lists:map(fun(F) -> mklink(Safer, F) end, Files)}
    end
  catch
    _:R -> error_logger:error_report([{fail, R}, {path, Path}])
  end.

only_md(F) ->
  case lists:reverse(F) of
    "dm."++_ -> true;
    _ -> false
  end.

mklink(Dirname, Filename) ->
  ["<a href=", Dirname, "/", Filename, ">", Filename, "</a><p>"].

markdown_convert(Filename, MD) ->
  case os:find_executable("pandoc") of
    false -> MD;
    Pandoc ->
      P = erlang:open_port({spawn, Pandoc++" -t html -f markdown-smart "++Filename}, []),
      receive
        {P, {data, HTML}} ->
          HTML
      after
        8000 ->
          MD
      end
  end.

a404(Meth, Path, QS) ->
  ["<h3>I'm confused; this doesn't make sense to me:</h3><p>",
   Meth, $:, lists:join($, , Path), $:, [[${, K, $, , V, $}] || {K, V} <- QS],
   "<p>I'm just a raspberry pi :<"].

ship(Status, State, ContentType, Body) ->
  {Status, State, content_type(ContentType), Body}.

content_type(text) -> [{<<"content-type">>, <<"text/plain">>}];
content_type(html) -> [{<<"content-type">>, <<"text/html">>}];
content_type(js)   -> [{<<"content-type">>, <<"application/javascript">>}];
content_type(css)  -> [{<<"content-type">>, <<"text/css">>}];
content_type(jpg)  -> [{<<"content-type">>, <<"image/jpeg">>}];
content_type(png)  -> [{<<"content-type">>, <<"image/png">>}];
content_type(ico)  -> [{<<"content-type">>, <<"image/png">>}].

synched_ts() ->
  {{Y, Mo, D}, {H, Mi, S}} = synch(),
  io_lib:fwrite("~w-~.2.0w-~.2.0w ~.2.0w:~.2.0w:~.2.0w", [Y, Mo, D, H, Mi, S]).

synch() ->
  T = round(1000-element(3, erlang:timestamp())/1000),
  receive
  after T -> erlang:universaltime()
  end.

safe_relative_path(Path) ->
  safe_relative_path([binary_to_list(E) || E <- Path], []).

safe_relative_path(["/"|_], _)        -> unsafe;
safe_relative_path([".."|_], [])      -> unsafe;
safe_relative_path([".."|R], [_|Acc]) -> safe_relative_path(R, Acc);
safe_relative_path(["."|R], Acc)      -> safe_relative_path(R, Acc);
safe_relative_path([E|R], Acc)        -> safe_relative_path(R, [E|Acc]);
safe_relative_path([], [])            -> "index.html";
safe_relative_path([], Acc)           -> filename:join(lists:reverse(Acc)).

extension(Path) ->
  case string:tokens(Path, ".") of
    [_] -> no_extension;
    [_|R] -> string:join(R, ".")
  end.

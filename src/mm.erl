%% -*- mode: erlang; erlang-indent-level: 2 -*-
%%% Created : 27 Jun 2012 by mats cronqvist <masse@klarna.com>

%% @doc
%% the massema.net server
%% @end

-module(mm).
-author('mats cronqvist').
-export([start/0
         ,parse/1
         ,test/0
         ,do/2]).

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
   {handler_function,{?MODULE,do}},
   {mime_types,[{"html","text/html"},
                {"css","text/css"},
                {"ico","image/x-icon"},
                {"js","application/javascript"}]}].

%% called from the server
%% we can deliver the content in chunks by calling Act(Chunk).
%% the first chunk can be headers; [{Key,Val}]
%% if we don't want to handle the request, we do Act(defer)
%% if we crash, there will be a 404
do(Act,Req) ->
  case mustache_file(Req) of
    "" -> Act(defer);
    MF -> Act(mustache(MF))
  end.

mustache_file(Req) ->
  try
    {Name,_} = proplists:get_value(real_name,Req(data)),
    ".html" = filename:extension(Name),
    MF = filename:rootname(Name,".html")++".mustache",
    true = filelib:is_regular(MF),
    MF
  catch
    _:_ -> ""
  end.

mustache(MF) ->
  {ok,Bin} = file:read_file(MF),
  parse(binary_to_list(Bin)).

-record(ms,{state=off,cfrag="",frags=[],cfun="",funs=[]}).
parse(Str) ->
  try compile(parse(Str,#ms{}))
  catch throw:R -> error_logger:error_report(R),Str
  end.

%% 92 is the escape char, "\"
%% "\" is forbidden in mustache context. in html context, it escapes the next
%% character. E.g. "{\{" will become "{{", and "\\" will become "\".
parse([92|R],   #ms{state=on} = MS) -> error(escape,MS,R);
parse([92,C|R], #ms{state=off}= MS) -> parse(R,MS#ms{cfrag=[C|MS#ms.cfrag]});
parse([$},$}|R],#ms{state=off}= MS) -> error(terminator,MS,R);
parse([${,${|R],#ms{state=on} = MS) -> error(nesting,MS,R);
parse([$},$}|R],#ms{state=on} = MS) -> parse(R,off(MS));
parse([${,${|R],#ms{state=off}= MS) -> parse(R,on(MS));
parse([C|R],    #ms{state=on} = MS) -> parse(R,MS#ms{cfun=[C|MS#ms.cfun]});
parse([C|R],    #ms{state=off}= MS) -> parse(R,MS#ms{cfrag=[C|MS#ms.cfrag]});
parse([],       #ms{state=on} = MS) -> error(premature,MS,"");
parse([],       #ms{state=off}= MS) -> done(MS).

error(Err,#ms{state=on, frags=[F|_]},R) -> throw({Err,F++"^"++R});
error(Err,#ms{state=on, frags=[]},R)    -> throw({Err,""++"^"++R});
error(Err,#ms{state=off,funs =[F|_]},R) -> throw({Err,F++"^"++R});
error(Err,#ms{state=off,funs =[]},R)    -> throw({Err,""++"^"++R}).

on(MS) ->
  MS#ms{frags=[lists:reverse(MS#ms.cfrag)|MS#ms.frags],cfrag=[],state=on}.
off(MS) ->
  MS#ms{funs=[lists:reverse(MS#ms.cfun)|MS#ms.funs],cfun=[],state=off}.

done(#ms{frags=Frags,cfrag=CF,funs=Funs}) ->
  {lists:reverse([lists:reverse(CF)|Frags]),lists:reverse(Funs)}.

compile({[],[]}) -> [];
compile({[F1|Frags],Funs}) ->
  F1++cmp(Frags,Funs).

cmp([],[]) -> "";
cmp([Frag|Frags],[Fun|Funs]) ->
  Fun++Frag++cmp(Frags,Funs).

test() ->
  FN = filename:join([code:priv_dir(massema.net),test,test.mustache]),
  {ok,FD} = file:open(FN,[read]),
  try test(FD,io:get_line(FD,''))
  after file:close(FD)
  end.

test(_,eof) -> ok;
test(FD,Line) ->
  A = io:get_line(FD,''),
  R = parse(Line),
  try R = A
  catch _:{badmatch,_} -> error_logger:error_report([{got,R},{expected,A}])
  end,
  test(FD,io:get_line(FD,'')).

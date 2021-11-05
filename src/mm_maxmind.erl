%% -*- mode: erlang; erlang-indent-level: 4 -*-
-module(mm_maxmind).

-export(
   [import/0,
    lookup/1]).

-include_lib("kernel/include/logger.hrl").

%% reloader
-on_load(reloader/0).
-export([table_loop/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% API

lookup({A, B, C, D}) -> lookup([A, B, C, D]);
lookup([A, B, C, D]) -> lookup(<<A:8, B:8, C:8, D:8>>);
lookup(<<I:32>>) -> lookup(I);
lookup(IP) when is_integer(IP) -> table_cmd({ip, IP}).

import() ->
    start_table_owner(),
    map_reduce(fun import_file/1, maxmind_files()).

reloader() ->
    erlang:send_after(100, ?MODULE, reload),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% map/reduce

map_reduce(F, Es) ->
    reduce(lists:map(fun(E) -> spawn_monitor(fun() -> F(E) end) end, Es)).

reduce([]) -> ok;
reduce(Workers) ->
    receive
        {'DOWN', Ref, process, Pid, {ok, Info}} ->
            ?LOG_INFO(#{imported => Info}),
            reduce(Workers -- [{Pid, Ref}]);
        {'DOWN', Ref, process, Pid, Err} ->
            error(#{worker_err => Err, worker => {Pid, Ref}, workers => Workers})
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% storage

maxmind_dir() ->
    PRIV = code:priv_dir(lift(application:get_application(?MODULE))),
    hd(lists:reverse(
         lists:sort(
           filelib:wildcard(
             filename:join(
               [PRIV, 'maxmind', '*']))))).

maxmind_files() ->
    filelib:wildcard(filename:join([maxmind_dir(), "**", "*{IPv4,en}.csv"])).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% the importer

import_file(File) ->
    FileId = file_id(File),
    Table = table_cmd({create, FileId}),
    {ok, FD} = file:open(File, [read, raw, binary, read_ahead]),
    {ok, Headers} = file:read_line(FD),
    Ks = parse_line(Headers),
    Ctx = #{fd => FD, table => Table, keys => Ks, file_id => FileId},
    import_lines(Ctx),
    table_cmd({reg, {FileId, Table}}),
    exit({ok, FileId}).

import_lines(Ctx) ->
    import_line(Ctx, read_line(Ctx)).

import_line(_, eof) -> ok;
import_line(Ctx, {ok, Line}) ->
    Vs = parse_line(Line),
    handle_rec(Vs, Ctx),
    import_line(Ctx, read_line(Ctx)).

read_line(#{fd := FD}) ->
    file:read_line(FD).

%% a bespoke mini parser. handles a newline terminated list of
%% comma-separated strings, where strings can be double-quoted.

parse_line(Line) -> parse_line(nil, Line, value(), []).

parse_line(nil, <<10:8>>, V, Vs) -> lists:reverse([value(V)|Vs]);
parse_line(nil, <<$,:8, R/binary>>, V, Vs) -> parse_line(nil, R, value(), [value(V)|Vs]);
parse_line(nil, <<$":8, R/binary>>, V, Vs) -> parse_line(quote, R, V, Vs);
parse_line(quote, <<$":8, R/binary>>, V, Vs) -> parse_line(nil, R, V, Vs);
parse_line(State, <<C:8, R/binary>>, V, Vs) -> parse_line(State, R, value(C, V), Vs).

%% a value abstraction.
%% /0 -> inital value, /1 -> finalize value, /2 add C to value.

value() -> [].
value(V) -> list_to_binary(lists:reverse(V)).
value(C, V) -> [C|V].

handle_rec(Vs, #{table := Table, keys := Ks}) ->
    Rec = zip(Ks, Vs, #{}),
    ets:insert(Table, {key(Rec), Rec}).

key(#{network := Network}) -> Network;
key(#{geoname_id := Id}) -> Id.

file_id(File) ->
    [_, A, _, B|_] = re:split(filename:basename(File), "-|\\."),
    binary_to_atom(<<A/binary, "/", B/binary>>).

%% zip two lists to a map, with filtering
zip([], [], O) -> O;
zip([K|Ks], [V|Vs], O) -> zip(Ks, Vs, record(K, V, O)).

record(K, V, O) ->
    case K of
        <<"network">>                        -> O#{cidr => V, network => cidr_to_range(V)};
        <<"autonomous_system_number">>       -> O#{asn => safe(integer, V)};
        <<"autonomous_system_organization">> -> O#{org => V};
        <<"geoname_id">>                     -> O#{geoname_id => V};
        <<"registered_country_geoname_id">>  -> O#{country_id => V};
        <<"postal_code">>                    -> O#{zip => V};
        <<"latitude">>                       -> O#{latitude => safe(float, V)};
        <<"longitude">>                      -> O#{longitude => safe(float, V)};
        <<"accuracy_radius">>                -> O#{accuracy => safe(integer, V)};
        <<"time_zone">>                      -> O#{tz => V};
        <<"is_anonymous_proxy">>             -> O#{is_anon => safe(boolean, V)};
        <<"is_satellite_provider">>          -> O#{is_sat => safe(boolean, V)};
        <<"is_in_european_union">>           -> O#{is_eu => safe(boolean, V)};
        <<"continent_name">>                 -> O#{continent_name => V};
        <<"country_name">>                   -> O#{country_name => V};
        <<"subdivision_1_name">>             -> O#{subdivision_1_name => V};
        <<"subdivision_2_name">>             -> O#{subdivision_2_name => V};
        <<"city_name">>                      -> O#{city_name => V};
        _ -> O
    end.

%% handle unreliable input
safe(_, <<>>)          -> undefined;
safe(boolean, <<"0">>) -> false;
safe(boolean, <<"1">>) -> true;
safe(integer, B)       -> binary_to_integer(B);
safe(float, B)         -> binary_to_float(B).
     
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% talk to the table owner

table_cmd(Cmd) ->
    ?MODULE ! {Cmd, self()},
    receive
        {ok, R} -> R;
        Err -> error(Err)
    after
        1000 -> error({create_table_fail, timeout})
    end.

start_table_owner() ->
    case whereis(?MODULE) of
        undefined -> start_owner();
        Pid -> Pid
    end.

start_owner() ->
    {Pid, Ref} = spawn_monitor(fun table_owner/0),
    Pid ! {start, Ref, self()},
    receive
        {started, Ref} -> erlang:demonitor(Ref), Pid;
        Err -> error(Err)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% table owner

table_owner() ->
    register(?MODULE, self()),
    ets:new(?MODULE, [named_table]),
    receive {start, Ref, From} -> From ! {started, Ref} end,
    table_loop(#{}).

table_loop(S) ->
    receive
        quit -> ok;
        reload -> ?MODULE:table_loop(S);
        What -> table_loop(handle(What))
    end.

handle({{reg, {Name, Ref}}, From}) ->
    From ! {ok, ets:insert(?MODULE, {Name, Ref})};
handle({{ip, IP}, From}) ->
    From ! {ok, maps:merge(lookup('ASN/IPv4', IP), get_geo(lookup('City/IPv4', IP)))};
handle({{create, Name}, From}) ->
    From ! {ok, ets:new(Name, [ordered_set, public])};
handle({{destroy, Name}, From}) ->
    From ! {ok, ets:delete(Name)}.

lookup(Tab, IP) ->
    [{_, Table}] = ets:lookup(?MODULE, Tab),
    case ets:lookup(Table, ets:prev(Table, {IP+1, 0})) of
        [{{IP0, IP1}, Val}] when IP0 =< IP, IP =< IP1 -> Val;
        _ -> #{}
    end.

get_geo(IPrec) ->
    [{_, Table}] = ets:lookup(?MODULE, 'City/en'),
    case ets:lookup(Table, maps:get(geoname_id, IPrec, '')) of
        [{_, GEOrec}] -> maps:merge(IPrec, GEOrec);
        [] -> IPrec
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% utils

% lift all the things
lift({error, X}) -> error(X);
lift({_, R}) -> R.

%% map <<"1.2.3.4/30">> to {16#01020304, 16#01020307}
cidr_to_range(CIDR) ->
    [IP, M] = binary:split(CIDR, <<"/">>),
    I = ip_string2int(IP),
    Mask = 16#FFFFFFFF bsr binary_to_integer(M),
    {I, I+(16#FFFFFFFF band Mask)}.

%% <<"1.1.1.1">> -> 16843009
ip_string2int(IP) ->
    binary:decode_unsigned(<<<<I:8>> || I <- ip_string2ilist(IP)>>).

%% <<"1.2.3.4">> -> [1,2,3,4]
ip_string2ilist(IP) ->
    [binary_to_integer(B) || B <- binary:split(IP, <<".">>, [global])].

%%ip_int2hex(I) ->
%%    [if C<10 -> C+$0; true -> C+$W end || <<C:4>> <= <<I:32>>].

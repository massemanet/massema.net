%% -*- mode: erlang; erlang-indent-level: 4 -*-
-module(mm_maxmind).

-export(
   [import/0,
    lookup/1]).

lookup(IP) ->
    table(ip, IP).

import() ->
    parallel(fun import_file/1, maxmind_files()).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% run funs in prallel
parallel(F, Es) ->
    Workers = lists:map(fun(E) -> spawn_monitor(fun() -> F(E) end) end, Es),
    wait_for_workers(Workers).

wait_for_workers([]) -> ok;
wait_for_workers(Workers) ->
    receive
        {'DOWN', Ref, process, Pid, normal} ->
            wait_for_workers(Workers -- [{Pid, Ref}]);
        {'DOWN', Ref, process, Pid, Err} ->
            error(#{worker_err => Err, worker => {Pid, Ref}, wporkers => Workers})
        end.

maxmind_dir() ->
    PRIV = code:priv_dir(lift(application:get_application(?MODULE))),
    hd(lists:reverse(
         lists:sort(
           filelib:wildcard(
             filename:join(
               [PRIV, 'maxmind', '*']))))).

maxmind_files() ->
    filelib:wildcard(filename:join([maxmind_dir(), "**", "*{IPv4,en}.csv"])).

import_file(File) ->
    erlang:spawn_monitor(fun() -> import_file_worker(File) end).

import_file_worker(File) ->
    FileId = file_id(File),
    Table = table(create, FileId),
    {ok, FD} = file:open(File, [read, raw, binary, read_ahead]),
    {ok, Headers} = file:read_line(FD),
    Ks = parse_line(Headers),
    Ctx = #{fd => FD, table => Table, keys => Ks, file_id => FileId},
    import_lines(Ctx),
    table(reg, {FileId, Table}).

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

parse_line(nil, <<10:8>>, V, Vs) -> lists:reverse([V|Vs]);
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

zip([], [], O) -> O;
zip([K|Ks], [V|Vs], O) -> zip(Ks, Vs, record(K, V, O)).

record(K, V, O) ->
    case K of
        <<"network">>                        -> O#{network => cidr_to_range(V)};
        <<"autonomous_system_number">>       -> O#{as => safe(integer, V)};
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

%% map <<"1.2.3.4/30">> to {16#01020304, 16#01020307}
cidr_to_range(CIDR) ->
    [IP, M] = binary:split(CIDR, <<"/">>),
    <<I:32>> = <<<<I:8>> || I <- [binary_to_integer(B) || B <- binary:split(IP, <<".">>, [global])]>>,
    Mask = 16#FFFFFFFF bsr binary_to_integer(M),
    {I, I+(16#FFFFFFFF band Mask)}.

safe(_, <<>>) -> undefined;
safe(boolean, <<"0">>) -> false;
safe(boolean, <<"1">>) -> true;
safe(integer, B) -> binary_to_integer(B);
safe(float, B) -> binary_to_float(B).
     
%% table owner

table(create, Name) ->
    table_cmd({create, Name});
table(destroy, Name) ->
    table_cmd({destroy, Name});
table(reg, {Name, Ref}) ->
    table_cmd({reg, {Name, Ref}});
table(ip, IP) ->
    table_cmd({ip, IP}).

table_cmd(Cmd) ->
    Pid = assert_owner(),
    Ref = erlang:monitor(process, Pid),
    Pid ! {Cmd, self()},
    receive
        {ok, R} -> erlang:demonitor(Ref), R;
        Err -> error(Err)
    after
        1000 -> error({create_table_fail, timeout})
    end.

assert_owner() ->
    case whereis(?MODULE) of
        undefined -> wait_for_owner();
        Pid -> Pid
    end.

wait_for_owner() ->
    {Pid, Ref} = spawn_monitor(fun table_owner/0),
    receive
        started -> erlang:demonitor(Ref), Pid;
        Err -> error(Err)
    end.

table_owner() ->
    register(?MODULE, self()),
    ets:new(?MODULE, [named_table]),
    lists:foreach(fun(P) -> P ! started end, lift(process_info(self(), monitored_by))),
    table_loop().

table_loop() ->
    receive
        quit -> ok;
        What -> handle(What), table_loop()
    end.

handle({{reg, {Name, Ref}}, From}) ->
    ets:insert(?MODULE, {Name, Ref}),
    From ! {ok, ok};
handle({{ip, IP}, From}) ->
    [{_, ASN}] = ets:lookup(?MODULE, 'ASN/IPv4'),
    [{_, IPV4}] = ets:lookup(?MODULE, 'City/IPv4'),
    [{_, EN}] = ets:lookup(?MODULE, 'City/en'),
    KeyIP = ets:prev(IPV4, {IP, 0}),
    KeyASN = ets:prev(ASN, {IP, 0}),
    [{_, IPrec}] = ets:lookup(IPV4, KeyIP),
    [{_, ASNrec}] = ets:lookup(ASN, KeyASN),
    [{_, GEOrec}] = ets:lookup(EN, maps:get(geoname_id, IPrec)),
    From ! {ok, maps:merge(GEOrec, maps:merge(IPrec, ASNrec))};
handle({{create, Name}, From}) ->
    From ! {ok, ets:new(Name, [ordered_set, public])};
handle({{destroy, Name}, From}) ->
    From ! {ok, ets:delete(Name)}.

lift({error, X}) -> error(X);
lift({_, R}) -> R.

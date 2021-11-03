%% -*- mode: erlang; erlang-indent-level: 4 -*-
-module(mm_maxmind).

-export([import/0]).

maxmind_dir() ->
    LIFT = fun({ok, X}) -> X end,
    PRIV = code:priv_dir(LIFT(application:get_application(?MODULE))),
    hd(lists:reverse(
         lists:sort(
           filelib:wildcard(
             filename:join(
               [PRIV, 'maxmind', '*']))))).

maxmind_files() ->
    filelib:wildcard(filename:join([maxmind_dir(), "**", "*{IPv4,en}.csv"])).

import() ->
    lists:map(mk_import_file(), maxmind_files()).

mk_import_file() ->
    {ok, MP} = re:compile(atom_to_list(',|\n')),
    fun(File) -> import_file(File, MP) end.

import_file(File, MP) ->
    {ok, FD} = file:open(File, [read, raw, binary, read_ahead]),
    {ok, Headers} = file:read_line(FD),
    Ks = re:split(Headers, MP, [trim]),
    {ok, Line} = file:read_line(FD),
    Vs = re:split(Line, MP, [trim]),
    zip(Ks, Vs, #{file => file_id(File)}).

file_id(File) ->
    [_, A, _, B|_] = re:split(filename:basename(File), "-|\\."),
    <<A/binary, "/", B/binary>>.
    

zip([], [], O) -> O;
zip([K|Ks], [V|Vs], O) -> zip(Ks, Vs, record(K, V, O)).

record(K, V, O) ->
    case K of
        <<"network">> -> O#{network => cidr_to_range(V)};
        <<"geoname_id">> -> O#{geoname_id => maybe_dq(V)};
        <<"continent_name">> -> O#{continent_name => maybe_dq(V)};
        <<"country_name">> -> O#{country_name => maybe_dq(V)};
        <<"subdivision_1_name">> -> O#{subdivision_1_name => maybe_dq(V)};
        <<"subdivision_2_name">> -> O#{subdivision_2_name => maybe_dq(V)};
        <<"city_name">> -> O#{city_name => maybe_dq(V)};
        _ -> O
    end.

cidr_to_range(CIDR) ->
    [A, B, C, D, M] = lists:map(fun binary_to_integer/1, re:split(CIDR, "/|\\.")),
    <<I:M/bitstring, _/binary>> = <<A:8, B:8, C:8, D:8>>,
    I.
    

maybe_dq(Bin) ->
    Size = byte_size(Bin),
    case 0 < Size andalso binary:first(Bin) of
        $" ->  binary:part(Bin, 1, Size-2);
        _ -> Bin
    end.

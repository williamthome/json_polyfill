-module(json_polyfill_SUITE).
-behaviour(ct_suite).
-include_lib("stdlib/include/assert.hrl").
-compile([export_all, nowarn_export_all]).

all() ->
    [encode, decode].

encode(Config) when is_list(Config) ->
    ?assertEqual([$", <<"foo">>, $"], json:encode(foo)).

decode(Config) when is_list(Config) ->
    ?assertEqual(<<"foo">>, json:decode(<<"\"foo\"">>)).

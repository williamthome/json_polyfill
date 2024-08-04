-module(json_polyfill_SUITE).
-behaviour(ct_suite).
-include_lib("stdlib/include/assert.hrl").
-compile([export_all, nowarn_export_all]).

all() ->
    [encode, decode].

encode(Config) when is_list(Config) ->
    ExpectA = [91,<<"null">>,44,
        ["{",
            [[34,<<"foo">>,34],58,34,<<"bar">>,34],
            [[44,[34,<<"bar">>,34],58,34,<<"baz">>,34]],
        "}"],
    93],
    ExpectB = [91,<<"null">>,44,
        ["{",
            [[34,<<"bar">>,34],58,34,<<"baz">>,34],
            [[44,[34,<<"foo">>,34],58,34,<<"bar">>,34]],
        "}"],
    93],
    Result = json:encode([null, #{foo => bar, bar => baz}]),
    ?assert(Result =:= ExpectA orelse Result =:= ExpectB).

decode(Config) when is_list(Config) ->
    ExpectA = [null,#{<<"bar">> => <<"baz">>,<<"foo">> => <<"bar">>}],
    ExpectB = [null,#{<<"foo">> => <<"bar">>,<<"bar">> => <<"baz">>}],
    Result = json:decode(iolist_to_binary(
        [91,<<"null">>,44,
            ["{",
                [[34,<<"foo">>,34],58,34,<<"bar">>,34],
                [[44,[34,<<"bar">>,34],58,34,<<"baz">>,34]],
            "}"],
        93]
    )),
    ?assert(Result =:= ExpectA orelse Result =:= ExpectB).

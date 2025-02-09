# json_polyfill

This lib is just a copy of the `json` module proposed in [EEP 68](https://www.erlang.org/eeps/eep-0068) and [introduced in OTP 27](https://www.erlang.org/news/168#highlights-for-rc2) by [Michał Muskała](https://github.com/michalmuskala).

Its intent is to act as a polyfill for apps that cannot currently be updated to OTP 27 or higher.

The module and function names are exactly the same. Then, when your app supports OTP >= 27, just optionally remove this lib from the dependencies.

> [!NOTE]
>
> There is no problem keeping this lib on OTP >= 27 when using `rebar`. The `beam` file is dropped on compilation time.

## Requirements

OTP >= 24.

## Installation

### Erlang

```erlang
% rebar.config
{deps, [{json_polyfill, "~> 0.2"}]}.
```

### Elixir

```elixir
# mix.exs
def deps do
  [{:json_polyfill, "~> 0.2"}]
end
```

## Encode

### encode/1

Generates JSON corresponding to `Term`.

Supports basic data mapping:

| **Erlang**             | **JSON** |
|------------------------|----------|
| `integer() \| float()` | Number   |
| `true \| false `       | Boolean  |
| `null`                 | Null     |
| `binary()`             | String   |
| `atom()`               | String   |
| `list()`               | Array    |
| `#{binary() => _}`     | Object   |
| `#{atom() => _}`       | Object   |
| `#{integer() => _}`    | Object   |

This is equivalent to `encode(Term, fun json:encode_value/2)`.

#### Example

```erlang
> iolist_to_binary(json:encode(#{foo => <<"bar">>})).
<<"{\"foo\":\"bar\"}">>
```

### encode/2

Generates JSON corresponding to `Term`.

Can be customised with the `Encoder` callback. The callback will be recursively called for all the data to be encoded and is expected to return the corresponding encoded JSON as iodata.

Various `encode_*` functions in this module can be used to help in constructing such callbacks.

#### Example

An encoder that uses a heuristic to differentiate object-like lists of key-value pairs from plain lists:

```erlang
> encoder([{_, _} | _] = Value, Encode) -> json:encode_key_value_list(Value, Encode);
> encoder(Other, Encode) -> json:encode_value(Other, Encode).
> custom_encode(Value) -> json:encode(Value, fun(Value, Encode) -> encoder(Value, Encode) end).
> iolist_to_binary(custom_encode([{a, []}, {b, 1}])).
<<"{\"a\":[],\"b\":1}">>
```

## Format

### format/1

Generates formatted JSON corresponding to `Term`.
Similiar to `encode/1` but with added whitespaces for formatting.

```erlang
> io:put_chars(json:format(#{foo => <<"bar">>, baz => 52})).
{
  "baz": 52,
  "foo": "bar"
}
ok
```

### format/2

Generates formatted JSON corresponding to `Term`.
Equivalent to `format(Term, fun json:format_value/3, Options)` or `format(Term, Encoder, #{})`.

### format/3

Generates formatted JSON corresponding to `Term`.
Similar to `encode/2`, can be customised with the `Encoder` callback and `Options`.
`Options` can include 'indent' to specify number of spaces per level and 'max' which loosely limits
the width of lists.
The `Encoder` will get a 'State' argument which contains the 'Options' maps merged with other data
when recursing through 'Term'.
`format_value/3` or various `encode_*` functions in this module can be used
to help in constructing such callbacks.

```erlang
> formatter({posix_time, SysTimeSecs}, Encode, State) ->
    TimeStr = calendar:system_time_to_rfc3339(SysTimeSecs, [{offset, "Z"}]),
    json:format_value(unicode:characters_to_binary(TimeStr), Encode, State);
> formatter(Other, Encode, State) -> json:format_value(Other, Encode, State).
>
> Fun = fun(Value, Encode, State) -> formatter(Value, Encode, State) end.
> Options = #{indent => 4}.
> Term = #{id => 1, time => {posix_time, erlang:system_time(seconds)}}.
>
> io:put_chars(json:format(Term, Fun, Options)).
{
    "id": 1,
    "time": "2024-05-23T16:07:48Z"
}
ok
```

### format_value/3

Default format function used by `json:format/1`.
Recursively calls `Encode` on all the values in `Value`,
and indents objects and lists.

### format_key_value_list/3

Format function for lists of key-value pairs as JSON objects.
Accepts lists with atom, binary, integer, or float keys.

### format_key_value_list_checked/3

@doc Format function for lists of key-value pairs as JSON objects.
Accepts lists with atom, binary, integer, or float keys.
Verifies that no duplicate keys will be produced in the
resulting JSON object.

#### Errors

Raises `error({duplicate_key, Key})` if there are duplicates.

## Decode

### decode/1

Parses a JSON value from `Binary`.

Supports basic data mapping:

| **JSON** | **Erlang**             |
|----------|------------------------|
| Number   | `integer() \| float()` |
| Boolean  | `true \| false`        |
| Null     | `null`                 |
| String   | `binary()`             |
| Object   | `#{binary() => _}`     |

#### Errors

* `error(unexpected_end)` if `Binary` contains incomplete JSON value
* `error({invalid_byte, Byte})` if `Binary` contains unexpected byte or invalid UTF-8 byte
* `error({invalid_sequence, Bytes})` if `Binary` contains invalid UTF-8 escape

#### Example

```erlang
> json:decode(<<"{\"foo\": 1}">>).
#{<<"foo">> => 1}
```

### decode/3

Parses a JSON value from `Binary`.

Similar to `decode/1` except the decoding process can be customized with the callbacks specified in `Decoders`. The callbacks will use the `Acc` value as the initial accumulator.

Any leftover, unparsed data in `Binary` will be returned.

#### Default callbacks

All callbacks are optional. If not provided, they will fall back to implementations used by the `decode/1` function:

* for `array_start`: `fun(_) -> [] end`
* for `array_push`: `fun(Elem, Acc) -> [Elem | Acc] end`
* for `array_finish`: `fun(Acc, OldAcc) -> {lists:reverse(Acc), OldAcc} end`
* for `object_start`: `fun(_) -> [] end`
* for `object_push`: `fun(Key, Value, Acc) -> [{Key, Value} | Acc] end`
* for `object_finish`: `fun(Acc, OldAcc) -> {maps:from_list(Acc), OldAcc} end`
* for `float`: `fun erlang:binary_to_float/1`
* for `integer`: `fun erlang:binary_to_integer/1`
* for `string`: `fun (Value) -> Value end`
* for `null`: the atom `null`

#### Errors

* `error({invalid_byte, Byte})` if `Binary` contains unexpected byte or invalid UTF-8 byte
* `error({invalid_sequence, Bytes})` if `Binary` contains invalid UTF-8 escape
* `error(unexpected_end)` if `Binary` contains incomplete JSON value

#### Example

Decoding object keys as atoms:

```erlang
> Push = fun(Key, Value, Acc) -> [{binary_to_existing_atom(Key), Value} | Acc] end.
> json:decode(<<"{\"foo\": 1}">>, ok, #{object_push => Push}).
{#{foo => 1},ok,<<>>}
```

## License

Erlang/OTP is released under the [Apache License 2.0](http://www.apache.org/licenses/LICENSE-2.0).

> %CopyrightBegin%
>
> Copyright Ericsson AB 2010-2024. All Rights Reserved.
>
> Licensed under the Apache License, Version 2.0 (the "License");
> you may not use this file except in compliance with the License.
> You may obtain a copy of the License at
>
>     http://www.apache.org/licenses/LICENSE-2.0
>
> Unless required by applicable law or agreed to in writing, software
> distributed under the License is distributed on an "AS IS" BASIS,
> WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
> See the License for the specific language governing permissions and
> limitations under the License.
>
> %CopyrightEnd%

# json_polyfill

This lib is just a copy of the `json` module proposed in [EEP 68](https://www.erlang.org/eeps/eep-0068) and [introduced in OTP 27](https://www.erlang.org/news/168#highlights-for-rc2) by [Michał Muskała](https://github.com/michalmuskala).

Its intent is to act as a polyfill for apps that cannot currently be updated to OTP 27 or higher.

The module and function names are exactly the same. Then, when your app supports OTP >= 27, just optionally remove this lib from the dependencies.

> [!NOTE]
>
> There is no problem keeping this lib on OTP >= 27, with rebar3 or Mix. The `json.beam` file is dropped at compile time, so the OTP `json` module is used.

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

### rebar3 plugin

Some rebar3 commands need the `json` module inside rebar3 itself, e.g.
`rebar3 experimental manifest`, used by [ELP](https://whatsapp.github.io/erlang-language-platform/).
On OTP < 27, add this lib as a plugin to make it available there:

```erlang
% rebar.config
{plugins, [{json_polyfill, "~> 0.2"}]}.
```

## Usage

The API is the same as the [OTP `json` module](https://www.erlang.org/doc/apps/stdlib/json.html), so refer to its documentation for all functions, including formatting and the streaming decoder.

### Erlang

```erlang
1> iolist_to_binary(json:encode(#{foo => <<"bar">>})).
<<"{\"foo\":\"bar\"}">>
2> json:decode(<<"{\"foo\": [1, 2.5, null]}">>).
#{<<"foo">> => [1,2.5,null]}
```

### Elixir

```elixir
iex> :json.encode(%{foo: "bar"}) |> IO.iodata_to_binary()
"{\"foo\":\"bar\"}"
iex> :json.decode(~s({"foo": [1, 2.5, null]}))
%{"foo" => [1, 2.5, :null]}
```

## License

Erlang/OTP is released under the [Apache License 2.0](http://www.apache.org/licenses/LICENSE-2.0).

> %CopyrightBegin%
>
> Copyright Ericsson AB 2024-2025. All Rights Reserved.
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

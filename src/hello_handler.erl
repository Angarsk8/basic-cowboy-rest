-module(hello_handler).

-export([init/2]).
-export([content_types_provided/2]).
-export([hello_to_html/2]).
-export([hello_to_json/2]).
-export([hello_to_text/2]).

set_default(id, undefined) -> integer_to_binary(rand:uniform(1000));
set_default(name, undefined) -> <<"Andres">>;
set_default(_, Value) -> Value.

response(json, Id, Name) ->
    #{
        <<"data">> => #{
            <<"user">> => #{
                <<"id">> => set_default(id, Id),
                <<"name">> => set_default(name, Name)
            },
            <<"posts">> => [
                #{
                    <<"name">> => <<"hola">>
                },
                #{
                    <<"name">> => <<"world">>
                }
            ]
        }
    }.

prettify(Term) ->
    EncodedTerm = jsx:encode(Term),
    jsxn:prettify(EncodedTerm).

init(Req, Opts) ->
    {cowboy_rest, Req, Opts}.

content_types_provided(Req, State) ->
    {[
        {<<"text/html">>, hello_to_html},
        {<<"application/json">>, hello_to_json},
        {<<"text/plain">>, hello_to_text}
    ], Req, State}.

hello_to_html(Req0, State) ->
    Id = cowboy_req:binding(id, Req0),
    Name = cowboy_req:binding(name, Req0),
    {ok, Body} = hello_dtl:render([
        {name, set_default(name, Name)},
        {id, set_default(id, Id)}
    ]),
    Req = cowboy_req:reply(200
        , #{<<"content-type">> => <<"text/html">>}
        , Body
        , Req0
    ),
    {ok, Req, State}.

hello_to_json(Req0, State) ->
    Id = cowboy_req:binding(id, Req0),
    Name = cowboy_req:binding(name, Req0),
    Req = cowboy_req:reply(200
        , #{<<"content-type">> => <<"application/json">>}
        , prettify(response(json, Id, Name))
        , Req0
    ),
    {ok, Req, State}.

hello_to_text(Req, State) ->
    {<<"REST Hello World as text!">>, Req, State}.

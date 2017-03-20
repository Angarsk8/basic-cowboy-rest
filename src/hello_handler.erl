-module(hello_handler).

-export([init/2]).
-export([content_types_provided/2]).
-export([hello_to_html/2]).
-export([hello_to_json/2]).
-export([hello_to_text/2]).
-export([proplist_to_map/1]).

set_default(id, undefined) -> integer_to_binary(rand:uniform(1000));
set_default(name, undefined) -> <<"Andres">>;
set_default(_, Value) -> Value.

prettify(Term) ->
  EncodedTerm = jsx:encode(Term),
  jsx:prettify(EncodedTerm).

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
  {ok, Body} = hello_html_dtl:render([
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
  Id    = cowboy_req:binding(id, Req0),
  Name  = cowboy_req:binding(name, Req0),
  User  = [{user, [{id, set_default(id, Id)}
                  ,{name, set_default(name, Name)}]}],
  Posts = [{posts, get_posts()}],
  Body  = [{data, User ++ Posts}],
  Req   = cowboy_req:reply(200
    , #{<<"content-type">> => <<"application/json">>}
    , prettify(Body)
    , Req0
  ),
	{ok, Req, State}.

get_posts() ->
  [
    [{id, 1}, {name, <<"Hello">>}
    ,{comments, [[{id, 1}, {title, <<"Hola">>}]]}],
    [{id, 2}, {name, <<"World">>}
    ,{comments, [[{id, 2}, {title, <<"Mundo">>}]]}]
  ].

hello_to_text(Req, State) ->
	{<<"REST Hello World as text!">>, Req, State}.

-spec proplist_to_map(proplists:proplist()) -> map().
proplist_to_map(Proplist) ->
  case is_proplist(Proplist) of
    true ->
      proplist_to_map(Proplist, #{});
    false ->
      erlang:error('bad argument')
  end.

-spec proplist_to_map(any(), #{}) -> map().
proplist_to_map(Term, Map) ->
  case is_proplist(Term) of
    true ->
      lists:foldl(
        fun({Key, Value}, Acc) ->
          maps:put(Key, proplist_to_map(Value, #{}), Acc)
        end,
        Map,
        Term
      );
    false ->
      Term
  end.

-spec is_proplist(any()) -> boolean().
is_proplist([]) -> true;
is_proplist([{Key,_} | Tail]) when is_atom(Key) -> is_proplist(Tail);
is_proplist(_) -> false.

-module(mechanic_headers).

%% External
-export([parse/1]).

%% External -----------------------------------------------------

-spec parse(binary()) -> {atom(), string(), list({atom(), string()})}.    

parse(Bin) ->
    case erlang:decode_packet(http, Bin, []) of
	{ok, {http_request, Method, Uri, _}, Rest} ->
	    parse(Method, Uri, get_headers(Rest, []));
	_ ->
	    {'GET', "/", []}
    end.


%% Internal -----------------------------------------------------

-spec parse(atom(), any(), list({atom(), string()})) -> {atom(), string(), list({atom(), string()})}.    

parse(Method, {_, _, _, _, Url}, Headers) ->
    {Method, Url, Headers};
parse(Method, {abs_path, Url}, Headers) ->
    {Method, Url, Headers};
parse(Method, _, Headers) ->
    {Method, "/", Headers}.


-spec get_headers(binary(), list()) -> list({atom(), string()}).    

get_headers(Bin, Acc) ->
    case erlang:decode_packet(httph, Bin, []) of
	{ok, {http_header, _, Header, _, Value}, Rest} when is_list(Header) ->
	    get_headers(Rest, [{list_to_atom(Header), Value} | Acc]);
        {ok, {http_header, _, Header, _, Value}, Rest} ->
            get_headers(Rest, [{Header, Value} | Acc]);
        _ ->
	    Acc
    end.

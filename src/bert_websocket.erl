-module(bert_websocket).
-behaviour(gen_socket).


%% Callbacks
-export([init/1, 
	 encode/1, 
	 decode/1,
	 on_open/0, 
	 on_send/1, 
	 on_recv/1, 
	 on_error/1,
	 on_close/1]).
 
 
%% Callbacks ----------------------------------------------------
 
-spec init(_) -> 'ok'.    

init(_) ->
    logger:write("initialising ~p, ~w", [?MODULE, self()]),
    ok.

encode(Term) ->
    List = binary_to_list(term_to_binary(encode_term(Term))),
    Unicode = unicode:characters_to_binary(List),
    mechanic_frame:reframe(Unicode).
 
decode(Bin) ->
    List = mechanic_frame:unframe(Bin),
    Term = binary_to_term(truncate(List)),
    decode_term(Term).

on_open() ->
    logger:write("on_open"),
    gen_socket:send(self(), atom_to_list(?MODULE)),
    noreply.

on_send(Encoded) ->
    logger:write("on_send: ~p", [decode(Encoded)]),
    noreply.

on_recv(Decoded) ->
    logger:write("on_recv: ~p", [Decoded]),
    gen_socket:send(self(), Decoded),
    noreply.
 
on_error(Error) ->
    logger:write("on_error: ~p", [Error]),
    noreply.    

on_close(Reason) ->
    logger:write("on_close: ~p", [Reason]),
    normal.

 
%% Internal -----------------------------------------------------
 

truncate(<<_, T/binary>>) -> T;
truncate(Any) -> Any.


-spec encode_term(term()) -> term().
%% Encode 
encode_term(Term) ->
    case Term of
	[] -> 
	    {bert, nil};
	true -> 
	    {bert, true};
	false -> 
	    {bert, false};
	Dict when is_record(Term, dict, 9) ->
	    {bert, dict, encode_term(dict:to_list(Dict))};
	List when is_list(Term) ->
	    lists:map((fun encode_term/1), List);
	Tuple when is_tuple(Term) ->
	    TList = tuple_to_list(Tuple),
	    TList2 = lists:map((fun encode_term/1), TList),
	    list_to_tuple(TList2);
	_ -> 
	    Term
    end.

 
-spec decode_term(term()) -> term().
%% Decode 
decode_term(Term) ->
    case Term of
	{bert, nil} -> 
	    [];
	{bert, true} -> 
	    true;
	{bert, false} -> 
	    false;
	{bert, dict, Dict} ->
	    dict:from_list(decode_term(Dict));
	{bert, Other} ->
	    {bert, Other};
	List when is_list(Term) ->
	    lists:map((fun decode_term/1), List);
	Tuple when is_tuple(Term) ->
	    TList = tuple_to_list(Tuple),
	    TList2 = lists:map((fun decode_term/1), TList),
	    list_to_tuple(TList2);
	_ ->
	    Term
    end.  

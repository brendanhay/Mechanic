-module(armstrong_websocket).
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
    mechanic_frame:reframe([Term]).

decode(Bin) ->
    mechanic_frame:unframe(Bin).

on_open() ->
    logger:write("on_open"),
    gen_socket:send(self(), atom_to_list(?MODULE)),
    mechanic_channel:connect(armstrong, self()),
    noreply.

on_send(Encoded) ->
    Decoded = decode(Encoded),
    logger:write("on_send: ~s", [Decoded]),
    mechanic_channel:broadcast(armstrong, Decoded), 
    noreply.
 
on_recv(Decoded) ->
    logger:write("on_recv: ~s", [Decoded]),
    gen_socket:send(self(), Decoded),
    noreply.
 
on_error(Error) ->
    logger:write("on_error: ~p", [Error]),
    noreply.    

on_close(Reason) ->
    logger:write("on_close: ~p", [Reason]),
    normal.

 

-module(tcp_socket).
-behaviour(gen_socket).


%% Callbacks
-export([init/1, 
	 encode/1, 
	 decode/1,
	 on_open/0, 
	 on_recv/1, 
	 on_error/1,
	 on_close/1]).
 

%% Callbacks ----------------------------------------------------
 
-spec init(_) -> 'ok'.    

init(_) ->
    logger:write("initialising ~p, ~w", [?MODULE, self()]),
    ok.


encode(Term) -> Term.

decode(Bin) -> Bin.

on_open() ->
    logger:write("on_open"),
    mechanic_channel:connect(tcp_socket, self()),
    gen_socket:send(self(), atom_to_list(?MODULE)),
    noreply.
 
on_recv(Decoded) ->
    logger:write("on_recv: ~s", [Decoded]),
    mechanic_channel:broadcast(tcp_socket, Decoded), 
    noreply.
 
on_error(Error) ->
    logger:write("on_error: ~p", [Error]),
    noreply.    

on_close(Reason) ->
    logger:write("on_close: ~p", [Reason]),
    mechanic_channel:disconnect(tcp_socket),
    normal.

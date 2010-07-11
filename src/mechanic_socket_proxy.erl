-module(mechanic_socket_proxy).

%% External
-export([start/1, start_link/1]).

%% Callbacks
-export([init/1]).

-define(DEFAULT_CLIENT, tcp_socket).

%% External -----------------------------------------------------

-spec start(port()) -> {'ok', pid()}.     

start(Socket) ->
    case supervisor:start_child(mechanic_socket_sup, [Socket]) of
    	{ok, Pid} ->
    	    gen_tcp:controlling_process(Socket, Pid);
    	{already_started, Pid} ->
    	    Pid;
    	Error ->
	    logger:write("error, ~p", [Error]),
    	    Pid = error	    
    end,
    {ok, Pid}.


-spec start_link(port()) -> {'ok', pid()}.

start_link(Socket) ->
    gen_socket:spawn_link(Socket, fun callback_factory/1).


%% Callbacks ----------------------------------------------------

-spec init(any()) -> 'ok'.    

init(_) ->
    ok.


%% Internal -----------------------------------------------------

-spec callback_factory(any()) -> {atom(), list(string())}.

callback_factory(Data) ->
    Request = mechanic_headers:parse(Data),
    Callback = determine_client(Request),
    Handshake = handshake(Request),
    logger:write("request: ~512p", [Request]), 
    {Callback, Handshake}.


-spec determine_client({atom(), string(), string()}) -> atom().    

determine_client({_, Url, _}) ->
    case application:get_env(mechanic, handlers) of
	{ok, Protocols} ->
	    proplists:get_value(Url, Protocols, ?DEFAULT_CLIENT);
	_ ->
	    ?DEFAULT_CLIENT
    end.


-spec handshake({atom(), string(), string()}) -> list(string()).

handshake({_, "/", _}) -> [];
handshake({_, Url, Headers}) ->
    Host = proplists:get_value('Host', Headers),
    Origin = proplists:get_value('Origin', Headers),
    logger:write("handshaking: ~s -> ~s", [Host, Origin]),
    Format = "HTTP/1.1 101 Web Socket Protocol Handshake\r\n" ++
	"Upgrade: WebSocket\r\n" ++
	"Connection: Upgrade\r\n" ++
	"WebSocket-Origin: ~s\r\n" ++
	"WebSocket-Location: ws://~s~s\r\n\r\n",
    io_lib:fwrite(Format, [Origin, Host, Url]).

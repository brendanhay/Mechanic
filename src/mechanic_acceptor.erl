-module(mechanic_acceptor).

%% External
-export([start/0, init/1]).

%% Connection Options
-define(TCP_OPTIONS, [binary, {packet, 0}, {active, true}, {reuseaddr, true}]).


%% External -----------------------------------------------------

-spec start() -> {'error', atom()} | {'ok', pid()}.

start() ->
    PortNumber = get_port(),
    logger:write("starting acceptor on port: ~w", [PortNumber]),
    case gen_tcp:listen(PortNumber, ?TCP_OPTIONS) of
	{ok, Port} ->
	    Pid = spawn_link(mechanic_acceptor, init, [Port]),
	    {ok, Pid};
	Error ->
	    Error
    end.


-spec init(port()) -> no_return().

init(Port) ->
    register(?MODULE, self()),
    logger:write("initialising, socket=~w", [Port]),
    listen(Port).


%% Internal -----------------------------------------------------

-spec listen(port()) -> no_return().

listen(Port) ->
    logger:write("listening, socket=~w", [Port]), 
    case gen_tcp:accept(Port) of
	{ok, Socket} ->
	    logger:write("connected, socket=~w", [Socket]), 
	    {ok, _} = mechanic_socket_proxy:start(Socket),
	    listen(Port);
	Reason ->
	    logger:write("stopping, reason=~w", [Reason]),
	    exit(Reason)
    end.


-spec get_port() -> any().
%% Gets the default listening port
get_port() ->
    case application:get_env(mech, port) of
	{ok, Port} ->
	    Port;
	undefined ->
	    1234
    end.


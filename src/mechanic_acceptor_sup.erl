-module(mechanic_acceptor_sup).
-behavior(supervisor_bridge).

%% supervisor_bridge interface
-export([start_link/0, terminate/2, init/1]).

-define(SERVER, {local, ?MODULE}).

start_link() -> 
    supervisor_bridge:start_link(?SERVER, ?MODULE, []).

init([]) ->
    logger:write("initialising"),
    case mechanic_acceptor:start() of  
	{ok, Pid} ->
	    {ok, Pid, Pid};
	Error ->
	    logger:write("failed to start ~p", [Error]),
	    Error
    end.	    

terminate(Reason, Pid) ->
    exit(Pid, Reason).

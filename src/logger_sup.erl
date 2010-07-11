-module(logger_sup).
-behavior(supervisor_bridge).

%% Callbacks
-export([start_link/0, terminate/2, init/1]).

-spec start_link() -> 'ignore' | {'error', _} | {'ok', pid()}.

start_link() -> 
    supervisor_bridge:start_link({local, ?MODULE}, ?MODULE, []).


-spec init([]) -> 'ignore' | {'error', _} | {'ok', pid(), pid()}.

init([]) ->
    case logger:start_link() of  
	{ok, Pid} ->
	    {ok, Pid, Pid};
	Error ->
	    Error
    end.	    


-spec terminate(_, pid() | port()) -> 'true'.

terminate(Reason, Pid) ->
    exit(Pid, Reason).

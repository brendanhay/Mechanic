-module(mechanic_socket_sup).
-behaviour(supervisor).

-export([start_link/0, init/1]).


-spec start_link() -> 'ignore' | {'error', _} | {'ok', pid()}.

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).


-spec init(_) -> {'ok', any()}.

init(Args) ->
    logger:write("intitialising"),
    Module = mechanic_socket_proxy,
    Child = {Module, {Module, start_link, Args},
	     temporary, brutal_kill, worker, [Module]},
    Spec = {{simple_one_for_one, 0, 1}, [Child]},
    {ok, Spec}.

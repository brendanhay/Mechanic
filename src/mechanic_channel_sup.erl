-module(mechanic_channel_sup).
-behavior(supervisor).

%% Callbacks
-export([start_link/0, init/1]).

%% Macros
-define(SERVER, {local, ?MODULE}).

%% Callbacks ----------------------------------------------------

start_link() ->
    supervisor:start_link(?SERVER, ?MODULE, []).

init(Args) ->
    logger:write("intitialising"),
    Module = mechanic_channel,
    Child = {Module, {Module, start_link, Args},
	     temporary, brutal_kill, worker, [Module]},
    Spec = {{simple_one_for_one, 0, 1}, [Child]},
    {ok, Spec}.

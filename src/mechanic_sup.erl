-module(mechanic_sup).
-behavior(supervisor).

%% Supervisor interace
-export([start_link/0, init/1]).


-spec start_link() -> 'ignore' | {'error', _} | {'ok', pid()}.

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).


-spec init(_) -> {'ok', any()}.

init(_) ->
    Children = [create_spec(logger_sup),
		create_spec(mechanic_acceptor_sup),
		create_spec(mechanic_socket_sup),
		create_spec(mechanic_channel_sup)],
    ok = supervisor:check_childspecs(Children),
    Spec = {{one_for_one, 2, 10}, Children},
    {ok, Spec}.


%% Internal -----------------------------------------------------

create_spec(Module) ->
    {Module, {Module, start_link, []},
     permanent, infinity, supervisor, [Module]}.

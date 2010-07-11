-module(mechanic).
-behaviour(application).

%% External
-export([start/0, start/2, stop/1]).


-spec start() -> 'ok' | {'error', _}.
%% Sugar for the OTP start mechanism
start() ->
    application:start(mechanic).


-spec start(_, _) -> 'ignore' | {'error', _} | {'ok', pid()}.

start(_, _) -> 
    io:fwrite("Starting mech application, pid=~w~n", [self()]),
    Res = mechanic_sup:start_link(),
    reloader:start_link(),    
    Res.


-spec stop(_) -> 'ok'.

stop(_) ->
    ok.

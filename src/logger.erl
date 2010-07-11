-module(logger).
-behaviour(gen_server).

%% External
-export([start_link/0, write/1, write/2]).

%% Callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, 
	 terminate/2, code_change/3]).

%% Records
-record(state, {}).


%% External -----------------------------------------------------

-spec start_link() -> 'ignore' | {'error', _} | {'ok', pid()}.

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


-spec write(_) -> 'ok'.

write(String) when is_list(String) ->
    gen_server:cast(?MODULE, {string, String, name(self())});
write(Bin) when is_binary(Bin) ->
    gen_server:cast(?MODULE, {string, binary_to_list(Bin), name(self())});
write(Term) ->
    gen_server:cast(?MODULE, {term, Term, name(self())}).


-spec write(string(), list()) -> ok.

write(Format, Args) ->
    gen_server:cast(?MODULE, {format, Format, Args, name(self())}).


%% Internal -----------------------------------------------------

-spec name(pid()) -> atom() | pid().

name(Pid) ->
    case process_info(Pid, registered_name) of
	{registered_name, Name} ->
	    Name;
	undefined ->
	    Pid;
	[] ->
	    Pid
    end.


-spec fwrite({'term', [any()], _} 
	     | {'format', string(), [any()], _}
	     | {'string', _, _}) -> 'ok'.

fwrite({term, Term, From}) ->
    fwrite({string, io_lib:fwrite("~w", Term), From});
fwrite({format, Format, Args, From}) ->
    fwrite({string, io_lib:fwrite(Format, Args), From});
fwrite({string, String, From}) ->
    io:fwrite("~s [~p] ~s~n", [timestamp(), From, String]).
 

-spec timestamp() -> string().
%% Creates a current timestamp 00:00:00 based on the local time
timestamp() ->
    {_, Times} = calendar:local_time(),
    Parts = [string:right(integer_to_list(T), 2, $0) || T <- tuple_to_list(Times)], 
    string:join(Parts, ":").

%% Callbacks ----------------------------------------------------

-spec init([]) -> {'ok', #state{}}.

init([]) ->
    {ok, #state{}}.


-spec handle_call(_, _, _) -> {'reply', 'ok', _}.

handle_call(_, _, State) ->
    {reply, ok, State}.


-spec handle_cast(_, _) -> {'noreply', _}.

handle_cast(Msg, State) ->
    fwrite(Msg),
    {noreply, State}.


-spec handle_info(_, _) -> {'noreply', _}.

handle_info(_, State) ->
    {noreply, State}.


-spec terminate(_, _) -> 'ok'.

terminate(_, _) ->
    ok.


-spec code_change(_, _, _) -> {'ok', _}.

code_change(_, State, _) ->
    {ok, State}.

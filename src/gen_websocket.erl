-module(gen_websocket).

%% External
-export([start_link/3, start_link/4, send/2]).

%% Callbacks 
-export([init_it/6]).

%% Behaviour Definition
-export([behaviour_info/1]).

-spec behaviour_info(_) -> undefined | [{atom(), non_neg_integer()}].

behaviour_info(callbacks) ->
    [{init, 1}, {encode, 1}, {decode, 1}, 
     {on_open, 0}, {on_message, 1}, {on_error, 1}, {on_close, 1}];
behaviour_info(_) ->
    undefined.

%% WebSocket States
-define(CONNECTING, 0).
-define(OPEN, 1).
-define(CLOSING, 2).
-define(CLOSED, 3).

%% The work flow (of the socket) can be described as follows:
%%
%%   User module                          Generic
%%   -----------                          -------
%%     start            ----->             start
%%     init             <-----              .
%%
%%                                         loop
%%
%%     on_open          <-----             open
%%
%%     on_message/1     <-----              .
%%
%%     on_error         <-----              .
%%
%%     close            ----->             close
%%     on_close         <-----             close
%%
%%     send             ----->             
%%

%% External -----------------------------------------------------

-spec start_link(atom(), list(), list()) -> ignore | {error, _} | {ok, pid()}.

start_link(Mod, Args, Options) ->
    gen:start(?MODULE, link, Mod, Args, Options).


-spec start_link({global | local, atom()}, atom(), list(), list()) -> ignore | {error, _} | {ok, pid()}.

start_link(Name, Mod, Args, Options) ->
    gen:start(?MODULE, link, Name, Mod, Args, Options).


-spec send(atom() | pid(), any()) -> any().    

send(Pid, Data) ->
    Pid ! {send, Data}.


%% Gen Callbacks ------------------------------------------------

-spec init_it(pid(), _, pid() | {_, _}, _, list(), _) -> no_return().

init_it(Starter, self, Name, Mod, Args, Options) ->
    init_it(Starter, self(), Name, Mod, Args, Options);
init_it(Starter, _, Name, Mod, [{socket, Socket}, {factory, Factory}|Args], _) ->
    case catch Mod:init(Args) of
	ok ->
	    proc_lib:init_ack(Starter, {ok, self()}), 	    
	    loop(Factory, Socket, ?CONNECTING);
	{stop, Reason} ->
	    %% For consistency, we must make sure that the
	    %% registered name (if any) is unregistered before
	    %% the parent process is notified about the failure.
	    %% (Otherwise, the parent process could get
	    %% an 'already_started' error if it immediately
	    %% tried starting the process again.
	    unregister_name(Name),
	    proc_lib:init_ack(Starter, {error, Reason}),
	    exit(Reason);
	ignore ->
	    unregister_name(Name),
	    proc_lib:init_ack(Starter, ignore),
	    exit(normal);
	{'EXIT', Reason} ->
	    unregister_name(Name),
	    proc_lib:init_ack(Starter, {error, Reason}),
	    exit(Reason);
	Else ->
	    Error = {bad_return_value, Else},
	    proc_lib:init_ack(Starter, {error, Error}),
	    exit(Error)
    end.


%% Internal -----------------------------------------------------

-spec unregister_name(pid() | {global | local, atom()}) -> any().

unregister_name({local, Name}) ->
    _ = (catch unregister(Name));
unregister_name({global, Name}) ->
    _ = global:unregister_name(Name);
unregister_name(Pid) when is_pid(Pid) ->
    Pid.


-spec loop(fun((any()) -> atom()) | atom(), port(), non_neg_integer()) -> none().

loop(Factory, Socket, ?CONNECTING) when is_function(Factory) ->
    receive
	Any ->
	    {Mod, Handshake} = Factory(Any)
    end,
    open(Handshake, Mod, Socket),
    loop(Mod, Socket, ?OPEN);
loop(Mod, Socket, ReadyState) when is_atom(Mod) ->
    Msg = receive
	      {tcp, Socket, Data} ->
		  {received, Data};
	      Any ->
		  Any
	  end,
    case handle(Msg, Mod, Socket, ReadyState) of
	?CLOSED ->
	    ok;
	_ ->
	    loop(Mod, Socket, ReadyState)
    end.


-spec handle({atom(), _}, atom(), port(), non_neg_integer()) -> non_neg_integer().

%% Receiving
handle({received, Data}, Mod, _, ReadyState=?OPEN) ->
    message(Data, Mod),
    ReadyState;
%% Sending
handle({send, _}, _, _, ReadyState=?CONNECTING) ->
    ReadyState;
handle({send, Data}, Mod, Socket, ReadyState=?OPEN) ->
    send(Data, Mod, Socket),
    ReadyState;
%% Socket closed
handle({tcp_closed, _}, Mod, Socket, _) ->
    close(tcp_closed, Mod, Socket),
    ?CLOSED.


-spec open(list(string()), atom(), port()) -> any().

open(Handshake, Mod, Socket) ->
    case gen_tcp:send(Socket, Handshake) of
	ok ->
	    Mod:on_open();
	Error ->
	    error(Error, Mod)
    end.


-spec message(_, atom() | tuple()) -> any().

message(Data, Mod) ->
    Mod:on_message(decode(Data, Mod)).


-spec send(_, atom(), port()) -> any().     

send(Data, Mod, Socket) ->
    Encoded = encode(Data, Mod),
    logger:write("writing ~p", [Encoded]),
    case gen_tcp:send(Socket, Encoded) of
	ok ->
	    noreply;
	Error ->
	    error(Error, Mod)
    end.


-spec error(_, atom()) -> any().

error(Error, Mod) ->
    Mod:on_error(Error).


-spec close(atom() | string(), atom(), port()) -> none().

close(Reason, Mod, Socket) ->
    case Reason of
    	tcp_closed ->
    	    ok;
    	_ ->
    	    gen_tcp:close(Socket)
    end,
    case Mod:on_close(Reason) of
	normal ->
	    exit(normal);
	shutdown ->
	    exit(shutdown);
	Shutdown = {shutdown, _} ->
	    exit(Shutdown);
	_ ->
	    exit(Reason)
    end.
    

-spec decode(_, atom()) -> any().

decode(Data, Mod) ->
    Unframed = unframe(Data),
    Mod:decode(Unframed).


-spec encode(_, atom()) -> any().

encode(Data, Mod) ->
    Encoded = Mod:encode(Data),
    reframe(Encoded).


-spec unframe(list() | binary()) -> list().    

unframe([0|T]) -> unframe_tail(T);
unframe(<<0, T/binary>>) -> unframe_tail(T, <<>>);
unframe(Any) -> Any.

unframe_tail([255]) -> [];
unframe_tail([H|T]) -> [H|unframe_tail(T)].

unframe_tail(<<255>>, Acc) -> Acc;
unframe_tail(<<H, T/binary>>, Acc) -> unframe_tail(T, <<Acc/binary, H>>).


-spec reframe(list()) -> list().    

reframe(List) when is_list(List) ->
    [0, List, 255];
reframe(Bin) when is_binary(Bin) ->
    Size = size(Bin),
    <<128, Size, Bin/binary>>.
 

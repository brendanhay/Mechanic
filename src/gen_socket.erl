-module(gen_socket).
-behaviour(plain_fsm).


% Commented due to errors using plain_fsm:parse_transform on windows
% -include("plain_fsm.hrl").


%% External
-export([spawn_link/2, send/2]).

%% Callbacks
-export([data_vsn/0, 
	 code_change/3]).

%% Called via hibernate()
-export([opening/1,
	 opened/1,
	 closing/1,
	 closed/1]).


%% Time to wait until considering the socket unused
-define(TIMEOUT, 200000).

%% Record to hold the impl module, and the socket
-record(state, {callback, socket, factory}).


%% Behaviour definition
-export([behaviour_info/1]).

-spec behaviour_info(_) -> undefined | [{atom(), non_neg_integer()}].

behaviour_info(callbacks) ->
    [{init, 1}, 
     {encode, 1},
     {decode, 1},
     {on_open, 0}, 
     {on_recv, 1}, 
     {on_error, 1}, 
     {on_close, 1}];
behaviour_info(_) ->
    undefined.


%% External -----------------------------------------------------

spawn_link(Socket, Factory) ->
    % Possibly need to transfer Pid ownership of the socket
    State = #state{socket=Socket, factory=Factory},
    Spawn = fun() ->
		    process_flag(trap_exit, true),
		    opening(State)
	    end,
    case plain_fsm:spawn_link(?MODULE, Spawn) of
	Pid when is_pid(Pid) ->
	    {ok, Pid};
	Error ->
	    {error, Error}
    end.


-spec send(atom() | pid(), any()) -> ok.    

send(Pid, Data) ->
    Pid ! {send, Data}.


%% Callbacks ----------------------------------------------------

data_vsn() -> 1.

code_change(_, State, _) ->
    {ok, {State, data_vsn()}}.


%% States -------------------------------------------------------

%%%
%%% Add away / idle state?
%%%

opening(State=#state{factory=Factory}) ->
    % Commented due to errors using plain_fsm:parse_transform on windows
    % plain_fsm:extended_receive(
    receive
	{tcp, Socket, Data} ->
	    {Callback, Handshake} = Factory(Data),
	    open(Callback, Socket, Handshake),
	    case Callback:init([]) of
		ok ->
		    opened(State#state{callback=Callback})
	    end;
	_ ->
	    close(unexpected)
    after 
	?TIMEOUT ->
	    close(timeout)
    end.

opened(State=#state{callback=Callback, socket=Socket}) ->
    receive
	{tcp, _, Data} ->
	    recv(Callback, Data),
	    opened(State);
	{send, Data} ->
	    case send(Callback, Socket, Data) of
		ok ->
		    opened(State);
		Error ->
		    error(Callback, Error),
		    closing(State)
	    end;
	{tcp_closed, _} ->
	    closing(State)
    after 
	?TIMEOUT ->
	    closing(State)
    end.

closing(State=#state{socket=Socket}) ->
    ok = gen_tcp:close(Socket),
    closed(State).

closed(#state{callback=Callback}) ->
    close(Callback, normal).


%% Internal -----------------------------------------------------

open(Callback, Socket, Handshake) ->
    Callback:on_open(),
    send(Callback, Socket, {encoded, Handshake}).

send(Callback, Socket, {encoded, Encoded}) ->
    case gen_tcp:send(Socket, Encoded) of
	ok ->
	    ok;
	Error ->
	    error(Callback, Error)
    end;
send(Callback, Socket, Data) ->
    Encoded = Callback:encode(Data),
    send(Callback, Socket, {encoded, Encoded}).

recv(Callback, Data) ->
    Decoded = Callback:decode(Data),
    Callback:on_recv(Decoded).

error(Callback, Error) ->
    Callback:on_error(Error).

close(Reason) ->
    exit(Reason).

close(Callback, Reason) ->
    Callback:on_close(Reason),
    exit(Reason).

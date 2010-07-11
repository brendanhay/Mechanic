-module(mechanic_channel).
-behaviour(gen_server).

%% External
-export([start_link/1, 
	 connect/2, 
	 disconnect/1,
	 broadcast/2]).

%% Callbacks
-export([init/1, 
	 handle_call/3, 
	 handle_cast/2, 
	 handle_info/2, 
	 terminate/2, 
	 code_change/3]).

%% Records
-record(state, {name, connections}).


%% External -----------------------------------------------------

%% Starts the server
start_link(Channel) ->
    gen_server:start_link({local, Channel}, ?MODULE, Channel, []).

%% Called by a user to join a channel
connect(Channel, Name) ->
    start(Channel),
    gen_server:cast(Channel, {join, self(), Name}).

disconnect(Channel) ->
    gen_server:cast(Channel, {part, self()}).

%% Broadcasts a message to all connected users
broadcast(Channel, Message) ->
    gen_server:cast(Channel, {message, self(), Message}).


%% Callbacks ----------------------------------------------------

%% Initiates the server
init(Channel) ->
    logger:write("initialising"),
    {ok, #state{name=Channel, connections=gb_trees:empty()}}.

%% No synchronous calls ;(
handle_call(_, _, State) ->
    {noreply, State}.

%% Handling all non call/cast messages
handle_info(_, State) ->
    {noreply, State}.


-spec terminate(_, _) -> 'ok'.    

terminate(_, _) ->
    ok.


-spec code_change(_, record(), _) -> {'ok', record()}.    

code_change(_, State, _) ->
    {ok, State}.


%% Commands -----------------------------------------------------

%% message
handle_cast({message, Pid, Message}, State) ->
    logger:write("message, ~p", [Message]),
    {noreply, message(Pid, Message, State)};
%% /join
handle_cast({join, Pid, Name}, State) ->
    logger:write("joined, ~p, ~p", [Pid, Name]),
    {noreply, join(Pid, Name, State)};
%% /part
handle_cast({part, Pid}, State) ->
    logger:write("parted, ~p", [Pid]),
    {noreply, part(Pid, State)};
%% /name
handle_cast({name, Pid, NewName}, State) ->
    logger:write("renamed, old=~p, new=~p", [Pid, NewName]),
    {noreply, name(Pid, NewName, State)};

%% Handle unknown command
handle_cast(_, State) ->
    {noreply, State}.   


%% Internal -----------------------------------------------------

-spec start(atom()) -> pid(). 
%% Called by connect/2
start(Channel) ->
    logger:write("starting ~p", [Channel]),
    case supervisor:start_child(mechanic_channel_sup, [Channel]) of
	{ok, Pid} ->
	    Pid;
	{error, {already_started, Pid}} ->
	    Pid
    end.

%% Messages all users in a channel
message(Name, Message, State) ->
    Formatted = format_message(State#state.name, Name, Message),
    Notify = fun(Pid) -> 
		     logger:write("sending to ~p", [Pid]), 
		     Pid ! {send, Formatted}
	     end,
    lists:map(Notify, gb_trees:keys(State#state.connections)),
    State.

%% Adds a user to the state 
join(Pid, Name, State) ->
    Updated = gb_trees:enter(Pid, Name, State#state.connections),
    State#state{connections=Updated}.

part(Pid, State) ->
    Updated = gb_trees:delete_any(Pid, State#state.connections),
    State#state{connections=Updated}.

%% Renames a connection
name(Pid, NewName, State) ->
    Updated = gb_trees:update(Pid, NewName, State#state.connections),
    State#state{connections=Updated}.

format_message(Channel, Name, Message) when is_binary(Message) ->
    format_message(Channel, Name, binary_to_list(Message));
format_message(Channel, Name, Message) ->
    io_lib:fwrite("[~p] ~p: ~s", [Channel, Name, Message]).

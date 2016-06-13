%% Contains the definition of protocol for
%% wall-chat

-module(wall_protocol).
-author(ppopoff).
-behaviour(gen_server).
-behaviour(ranch_protocol).
-include("wall.hrl").

%% API Function Exports
-export([start_link/4, stop/0]).

%% gen_server Function Exports
-export([
    init/1, init/4, handle_call/3, handle_cast/2,
    handle_info/2, terminate/2, code_change/3
]).



% Connection state
-record(state, {
    auth_status = false :: boolean(),
    was_dropped = false :: boolean(),
    username = <<>>     :: username(),
    socket              :: port(),
    transport           :: transport(),
    buffer = <<>>       :: binary()
}).


-type state() :: #state{}.
-type transport() :: any().
-type username()  :: binary().
-type message()   :: map().



%% @doc initializes and acceptor for the connection
start_link(Ref, Socket, Transport, Opts) ->
    proc_lib:start_link(?MODULE, init, [Ref, Socket, Transport, Opts]).


%% @doc It's here to satify gen_server behaviour
init([]) -> {ok, undefined}.


%% @doc stops the acceptor
stop() ->
    gen_server:cast(?MODULE, stop).


%% @doc Creates connection, and initial state
init(Ref, Socket, Transport, _Opts = []) ->
    proc_lib:init_ack({ok, self()}),
    ranch:accept_ack(Ref),
    Transport:setopts(Socket, [{active, once}]),
    NewState = #state {transport = Transport, auth_status = false, socket = Socket},
    gen_server:enter_loop(?MODULE, [], NewState, ?TIMEOUT).


%% @doc This code handles authentication
handle_info({tcp, Socket, Data}, State = #state {auth_status = false,
                                                 transport = Transport,
                                                 buffer = Buffer}) ->
    % Append received element to the buffer
    CurrentBuffer = <<Buffer/binary, Data/binary>>,

    % If some data is left it will be kept inside the buffer
    Transport:setopts(Socket, [{active, once}]),
    {noreply, decode_auth(CurrentBuffer, State), ?TIMEOUT};


%% @doc Receiving the message when user is authenticated
handle_info({tcp, Socket, Data}, State = #state {auth_status = true,
                                                 transport = Transport,
                                                 buffer = Buffer}) ->
    % Append received element to the buffer
    CurrentBuffer = <<Buffer/binary, Data/binary>>,

    % If some data is left it will be kept inside the buffer
    Transport:setopts(Socket, [{active, once}]),
    {noreply, decode_data(CurrentBuffer, State), ?TIMEOUT};


%% @doc Message broadcasting
handle_info({broadcast, Message}, State = #state {auth_status = true,
                                                  transport = Transport,
                                                  socket = Socket}) ->
    Transport:setopts(Socket, [{active, once}]),
    lager:info("To ~tp message: ~tp", [Socket, Message]),
    Transport:send(Socket, Message),
    {noreply, State};


%% @doc Cases when server will be stopped
handle_info(drop, State) ->
    {stop, normal, State#state{was_dropped = true}};
handle_info(_Info, State) ->
    {stop, normal, State}.

%% @hidden not supprted
handle_call(_Request, _From, State) ->
    {reply, ok, State}.


%% @doc Handles the unknown messages
-spec handle_cast(message(), state()) -> {noreply, state()}.
handle_cast(Message, State) ->
    lager:info("Unknown message: ~tp", [Message]),
    {noreply, State}.


%% @doc Cleans all the resources, removes the entries from ets tables
-spec terminate(any(), state()) -> ok.
terminate(_Reason, _State = #state {auth_status = false}) ->
    lager:info("Session was terminated, before user logged in.");
terminate(_Reason, _State = #state {auth_status = true, was_dropped = true}) ->
    lager:info("Connection was dropped.");
terminate(_Reason, State) ->
    case wall_users:exist(State#state.username) of
        true ->  lager:info("Removing the user ~tp", [State#state.username]),
                 wall_users:del(State#state.username);
        false -> lager:info("Session terminated")
    end, ok.


%% @hidden not supported
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% @doc validates user name
-spec is_valid_username(username()) -> boolean().
is_valid_username(<<"">>)       -> false;
is_valid_username(<<"server">>) -> false;
is_valid_username(<<"admin">>)  -> false;
is_valid_username(_Username)    -> true.


%% @doc Registers user in the database
-spec register_user(username(), port(), transport()) -> {'noreply', state()}.
register_user(Username, Socket, Transport) ->
    lager:info("No such user exist. Creating..."),
    Status = wall_users:reg(Username, self()),
    lager:debug("Registration status ~tp", [Status]),

    Transport:send(Socket, wall_message:auth_success()),
    {noreply, #state{
        auth_status = true,  socket = Socket,
        username = Username, transport = Transport
    }}.


%% @doc Reregisters user in the database
-spec reregister_user(username(), port(), transport()) -> {'noreply', state()}.
reregister_user(Username, Socket, Transport) ->
    lager:info("The specified user exist. Dropping..."),
    notify_and_drop_given_client(
        Username, "A new user with the same name connected. You'll be dropped"
    ),

    Status = wall_users:rreg(Username, self()),
    lager:debug("Reregistration status ~tp", [Status]),

    Transport:send(Socket, wall_message:auth_success()),
    {noreply, #state{
        auth_status = true,  socket = Socket,
        username = Username, transport = Transport
    }}.


%% @doc decodes the authentication message
-spec decode_auth(message(), state()) -> state().
decode_auth(Data = <<Size:?HEADER_SIZE/unsigned-big-integer, Rest/binary>>,
            State = #state{socket = Socket, transport = Transport}) ->
    case byte_size(Rest) >= Size of
         true  ->
               <<MessageBody:Size/binary, _BytesRem/binary>> = Rest,
               handle_auth(MessageBody, Socket, Transport),
               State#state{auth_status = true, buffer = <<>>};
         false -> % if no integral message received put it back to buf
               State#state{buffer=Data}
    end;
decode_auth(Buffer, State) ->
    State#state{buffer = Buffer}.


%% @doc handles authentication process
-spec handle_auth(message(), port(), transport()) -> ok.
handle_auth(MessageBody, Socket, Transport) ->
    case is_auth_successful(MessageBody) of
        {true, Username} ->
            case wall_users:exist(Username) of
                true  -> reregister_user(Username, Socket, Transport);
                false -> register_user(Username, Socket, Transport)
            end;
        {false, Reason} ->
            drop_given_client(self(), Reason)
    end.


%% @doc if Authentication is successful
-spec is_auth_successful(binary()) -> {true | message()} | {false | any()}.
is_auth_successful(MessageBody) ->
    case wall_message:decode(MessageBody) of
        {ok, Message} ->
            {ok, Username} = maps:find(?USER_FIELD, Message),
            lager:info("User ~tp is authenticating.", [Username]),

            case is_valid_username(Username) of
                true  -> {true, Username};
                false -> {false, "Username " ++ Username ++ " is invalid "}
            end;

       {failed, _ } ->
           {false, "Ivalid authentication request body"}
    end.


%% @doc Decodes and handles the message
-spec decode_data(message(), state()) -> state().
decode_data(Data = <<Size:?HEADER_SIZE/unsigned-big-integer, Rest/binary>>, State) ->
    case byte_size(Rest) >= Size of
         true  ->  <<MessageBody:Size/binary, BytesRem/binary>> = Rest,
                   handle_message(MessageBody),
                   decode_data(BytesRem, State#state{buffer=BytesRem});
         false ->  State#state{buffer=Data}
    end;
decode_data(Buffer, State) ->
    State#state{buffer=Buffer}.


%% @doc Decodes and sends the message to other users
-spec handle_message(message()) -> ok.
handle_message(MessageBody) ->
    case wall_message:decode(MessageBody) of
        {ok, Message} -> notify_other_clients(wall_message:with_timestamp(Message));
        {failed, _ }  -> lager:info("Message wasn't send due to illegal content")
    end.


%% @doc Sends broadcast message to all available clients
-spec notify_other_clients(message()) -> ok.
notify_other_clients(Message) when is_map(Message) ->
    broadcast_message(
        wall_users:active_connections_except(self()),
        wall_message:encode(Message)
    ).


%% @doc Broadcasts given message to other users
-spec broadcast_message (list(pid()), message()) -> ok.
broadcast_message([], _Message)        -> ok;
broadcast_message([Pid|Pids], Message) ->
    Pid ! {broadcast, Message},
    broadcast_message(Pids, Message).


%% @doc Sends a farewell to the user, drops and removes them from ets table
-spec notify_and_drop_given_client(username(), string()) -> ok.
notify_and_drop_given_client(Username, Reason) ->
    [{Username, {Pid, _Timestamp}}] = wall_users:find(Username),
    drop_given_client(Pid, Reason).


%% @doc Sends the final message (Reason) and drops the user
-spec drop_given_client(pid(), string()) -> ok.
drop_given_client(Pid, Reason) ->
    broadcast_message([Pid], wall_message:new("server", Reason)),
    Pid ! drop,
    ok.


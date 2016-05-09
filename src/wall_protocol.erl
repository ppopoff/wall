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


%% @doc initializes and acceptor for the connection
start_link(Ref, Socket, Transport, Opts) ->
    proc_lib:start_link(?MODULE, init, [Ref, Socket, Transport, Opts]).


%% @doc It's here to satify gen_server behaviour
init([]) -> {ok, undefined}.


%% @doc stops the acceptor
stop() ->
    lager:info("Stopping the listener"),
    gen_server:cast(?SERVER, stop).


%% @doc Creates connection, and initial state
init(Ref, Socket, Transport, _Opts = []) ->
    proc_lib:init_ack({ok, self()}),
    ranch:accept_ack(Ref),
    Transport:setopts(Socket, [{active, once}]),

    NewState = #state {transport = Transport, auth_status = false, socket = Socket},
    gen_server:enter_loop(?MODULE, [], NewState, ?TIMEOUT).


%% @doc This code handles authorization
handle_info(
    {tcp, Socket, Data},
    State = #state {auth_status = false, transport = Transport, buffer = Buffer}
) ->
    % Append received element to the buffer
    CurrentBuffer = <<Buffer/binary, Data/binary>>,

    % If some data is left it will be kept inside the buffer
    Transport:setopts(Socket, [{active, once}]),
    {noreply, decode_auth(CurrentBuffer, State), ?TIMEOUT};


%% @doc Receiving the message when user is authenticated
handle_info(
    {tcp, Socket, Data},
    State = #state {auth_status = true, transport = Transport, buffer = Buffer}
) ->
    % Append received element to the buffer
    CurrentBuffer = <<Buffer/binary, Data/binary>>,

    % If some data is left it will be kept inside the buffer
    Transport:setopts(Socket, [{active, once}]),
    {noreply, decode_data(CurrentBuffer, State), ?TIMEOUT};


%% @doc Message broadcasting
%% @spec handle_info({broadcast, message()}, state()) -> {noreply, state()}.
handle_info(
  {broadcast, Message},
  State = #state {auth_status = true, transport = Transport, socket = Socket}
) ->
    Transport:setopts(Socket, [{active, once}]),
    lager:info("To ~tp message: ~tp", [Socket, Message]),
    Transport:send(Socket, Message),
    {noreply, State};


%% @doc Cases when server will be stopped
handle_info({tcp_closed, _Socket}, State) ->
    {stop, normal, State};
handle_info({tcp_error, _, Reason}, State) ->
    {stop, Reason, State};
handle_info(timeout, State) ->
    {stop, normal, State};
handle_info(drop, State) ->
    {stop, normal, State#state{was_dropped = true}};
handle_info(stop, State) ->
    {stop, normal, State};
handle_info(_Info, State) ->
    {stop, normal, State}.

%% @hidden not supprted
handle_call(_Request, _From, State) ->
    {reply, ok, State}.


%% @doc Handles the unknown messages
%% @spec handle_cast(message(), state()) -> {noreply, state()}.
-spec handle_cast(message(), state()) -> {noreply, state()}.
handle_cast(Message, State) ->
    lager:info("Unknown message: ~tp", [Message]),
    {noreply, State}.


%% @doc Cleans all the resources, removes the entries from ets tables
%% @spec terminate(any(), state()) -> ok.
-spec terminate(any(), state()) -> ok.
terminate(_Reason, _State = #state {auth_status = false}) ->
    lager:info("Session was terminated, before user logged in."),
    ok;
terminate(_Reason, _State = #state {auth_status = true, was_dropped = true}) ->
    lager:info("Connection was dropped."),
    ok;
terminate(_Reason, State) ->
    case wall_users:exist(State#state.username) of
        true ->  lager:info("Removing the user ~tp", [State#state.username]),
                 wall_users:del(State#state.username),
                 ok;
        false -> lager:info("Session terminated"),
                 ok
    end.


%% @hidden not supported
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% @doc Registers user in the database
%% @spec register_user(username(), port(), transport()) -> {'noreply', state()}.
-spec register_user(username(), port(), transport()) -> {'noreply', state()}.
register_user(Username, Socket, Transport) ->
    Status = wall_users:reg(Username, self()),
    lager:debug("Registration status ~tp", [Status]),

    Transport:send(Socket, _AuthSuccess = <<"OK">>),
    {noreply, #state{
        auth_status = true,  socket = Socket,
        username = Username, transport = Transport
    }}.


%% @doc Reregisters user in the database
%% @spec reregister_user(username(), port(), transport()) -> {'noreply', state()}.
-spec reregister_user(username(), port(), transport()) -> {'noreply', state()}.
reregister_user(Username, Socket, Transport) ->
    notify_and_drop_given_client(
        Username,
        "A new user with following nickname connected. You will be dropped"
    ),

    Status = wall_users:rreg(Username, self()),
    lager:debug("Reregistration status ~tp", [Status]),

    Transport:send(Socket, _AuthSucess = <<"OK">>),
    {noreply, #state{
        auth_status = true,  socket = Socket,
        username = Username, transport = Transport
    }}.


%% @doc Sends broadcast message to all available clients
%% @spec notify_other_clients(message()) -> ok.
-spec notify_other_clients(message()) -> ok.
notify_other_clients(Message) when is_map(Message) ->
    ActiveClients = wall_users:active_connections_except(self()),
    broadcast_message(ActiveClients, wall_codec:encode_message(Message)).


%% @doc Sends a farewell to the user, drops and removes them from ets table
%% @spec notify_and_drop_given_client(username(), Reason :: string()) -> ok.
-spec notify_and_drop_given_client(username(), Reason :: string()) -> ok.
notify_and_drop_given_client(Username, Reason) ->
    [{Username, {Pid, _Timestamp}}] = wall_users:find(Username),
    drop_given_client(Pid, Reason).


%% @doc Sends the final message (Reason) and drops the user
%% @spec drop_given_client(Pid :: pid(), Reason :: string()) -> ok.
-spec drop_given_client(Pid :: pid(), Reason :: string()) -> ok.
drop_given_client(Pid, Reason) ->
    broadcast_message([Pid], wall_utils:message("server", Reason)),
    Pid ! drop,
    ok.


%% @doc Broadcasts given message to other users
%% @spec broadcast_message (list(pid()), message()) -> ok.
-spec broadcast_message (list(pid()), message()) -> ok.
broadcast_message([], _Message) ->
    ok;
broadcast_message([Pid|Pids], Message) ->
    Pid ! {broadcast, Message},
    broadcast_message(Pids, Message).



%% @doc decodes the authentication message
%% @spec decode_auth(message(), state()) -> state().
-spec decode_auth(message(), state()) -> state().
decode_auth(
    Data = <<Size:?HEADER_SIZE/unsigned-big-integer, Rest/binary>>,
    State = #state{socket = Socket, transport = Transport}
) ->
    case byte_size(Rest) >= Size of
         true  ->
               <<MessageBody:Size/binary, _BytesRem/binary>> = Rest,
               handle_auth(MessageBody, Socket, Transport),

               % Change server's state (authenticated),
               % and clean up the buffer
               State#state{auth_status = true, buffer = <<>>};
         false -> % No integral messages received
               % Put Data back into the buffer
               State#state{buffer=Data}
    end;
decode_auth(Buffer, State) ->
    State#state{buffer=Buffer}.



%% @doc handles authentication process
%% @spec handle_auth(message(), port(), transport()) -> ok.
-spec handle_auth(message(), port(), transport()) -> ok.
handle_auth(MessageBody, Socket, Transport) ->
    try wall_utils:deserialize(MessageBody) of
        Message ->
            {ok, AuthRequest} = maps:find(?MESSAGE_FIELD, Message),
            {ok, Username}    = maps:find(?USER_FIELD,    Message),

            lager:info("User with name ~tp is trying to authenticate", [Username]),
            lager:info("Checking username ~tp in the database", [Username]),

            case wall_users:exist(Username) andalso AuthRequest =:= "auth" of
                true ->
                    lager:info("The specified user exist. Dropping that user..."),
                    reregister_user(Username, Socket, Transport);
                false ->
                    case Username of
                        ""   -> Reason = "Users with empty name are not allowed",
                                lager:info(Reason),
                                drop_given_client(self(), Reason);
                        Name -> lager:info("no such user exist. Creating..."),
                                register_user(Name, Socket, Transport)
                    end
            end
    catch
        Exception -> lager:error(
            "Unable to authenticate the user, because of ~tp", [Exception]
        )
    end,
    ok.


%% @doc Decodes and handles the message
%% @spec decode_data(message(), state()) -> state().
-spec decode_data(message(), state()) -> state().
decode_data(Data = <<Size:?HEADER_SIZE/unsigned-big-integer, Rest/binary>>, State) ->
    case byte_size(Rest) >= Size of
         true  -> % At least one message was received
               <<MessageBody:Size/binary, BytesRem/binary>> = Rest,
               handle_message(MessageBody),
               % decode everything that's left
               decode_data(BytesRem, State#state{buffer=BytesRem});
         false -> % No integral messages received
               % Put Data back into the buffer
               State#state{buffer=Data}
    end;
decode_data(Buffer, State) ->
    State#state{buffer=Buffer}.


%% @doc Decodes and sends the message to other users
%% @spec handle_message(message()) -> ok.
-spec handle_message(message()) -> ok.
handle_message(MessageBody) ->
    try wall_utils:deserialize(MessageBody) of
        Message -> notify_other_clients(
            wall_utils:add_timestamp(Message))
    catch
        Exception -> lager:error(
            "Unable to handle message due to it's inappropriate format ~tp", [Exception])
    end,
    ok.


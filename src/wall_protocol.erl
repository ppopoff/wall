%%% -*- erlang -*-

-module(wall_protocol).
-behaviour(gen_server).
-behaviour(ranch_protocol).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/4]).
-export([stop/0]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1]).
-export([init/4]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).


-record(state, {
    auth_status = false :: boolean(),
    username = <<>>     :: binary(),
    socket              :: port(),
    transport           :: any(), %TODO: handle this type
    buffer = <<>>       :: binary()
}).

-type state() :: #state{}.


-define(AUTH_HEADER, 16).
-define(SERVER, ?MODULE).
-define(HEADER_SIZE, 24). % 3 bytes
-define(TIMEOUT, infinity).



start_link(Ref, Socket, Transport, Opts) ->
    proc_lib:start_link(?MODULE, init, [Ref, Socket, Transport, Opts]).


init([]) -> {ok, undefined}.


stop() ->
    lager:info("Stopping the listener"),
    gen_server:cast(?SERVER, stop).


init(Ref, Socket, Transport, _Opts = []) ->
    ok = proc_lib:init_ack({ok, self()}),
    ok = ranch:accept_ack(Ref),
    ok = Transport:setopts(Socket, [{active, once}]),
    %% Creating a state for the connection
    NewState = #state{socket=Socket, transport=Transport},
    gen_server:enter_loop(?MODULE, [], NewState, ?TIMEOUT).


%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------


%% This code handles authorization
%% About the authentication protocol:
%% user sends A message (username)
%% Message format:
%% Header: 2 bytes big Endian int (size of payload)
%% Payload
%% Client should get the following line in response
%% 'OK'
%%
handle_info({tcp, Socket, Data}, State=#state{auth_status = false}) ->
    Transport = State#state.transport,
    Socket    = State#state.socket,
    HeaderSize = 2,

    lager:info("A new user is coming"),
    Transport:setopts(Socket, [{active, once}]),

    <<Header:HeaderSize/binary, Rest/binary>> = Data,
    MessageLen = binary:decode_unsigned(Header),

    <<Username:MessageLen/binary, _Left/binary>> = Rest,
    lager:info("User with name ~tp is trying to authenticate", [Username]),
    lager:info("Checking username ~tp in the database", [Username]),

    case wall_users:exist(Username) of
        true ->
            lager:info("The specified user exist. Dropping that user..."),
            notify_and_drop_given_client(Username),
            register_user(Username, Socket, Transport, true);
        false ->
            lager:info("no such user exist. Creating..."),
            register_user(Username, Socket, Transport, false)
    end;



% @doc Receiving the message when user is authenticated
handle_info({tcp, Socket, Data}, State=#state{auth_status=true}) ->
    lager:info("Waiting for the header"),
    Transport = State#state.transport,
    Buffer    = State#state.buffer,

    %% Append received element to the buffer
    CurrentBuffer = <<Buffer/binary, Data/binary>>,

    % If some data is left it will be kept inside the buffer
    NewState = decode_data(CurrentBuffer, State),

    Transport:setopts(Socket, [{active, once}]),
    {noreply, NewState, ?TIMEOUT};


% @doc Message broadcasting
handle_info({broadcast, _Username, Message}, State=#state{auth_status=true}) ->
    Transport = State#state.transport,
    Socket    = State#state.socket,

    Transport:setopts(Socket, [{active, once}]),
    lager:info("To ~tp message: ~tp", [Socket, Message]),
    Transport:send(Socket, Message),
    {noreply, State};


% Cases when server will be stopped
handle_info({tcp_closed, _Socket}, State) ->
    {stop, normal, State};

handle_info({tcp_error, _, Reason}, State) ->
    {stop, Reason, State};

handle_info(timeout, State) ->
    {stop, normal, State};

handle_info(stop, State) ->
    {stop, normal, State};

handle_info(_Info, State) ->
    {stop, normal, State}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

% Unknown message accepted
handle_cast(_Msg, State) ->
    {noreply, State}.


terminate(_Reason, _State=#state{auth_status=false, username = <<>>}) ->
    lager:info("Session was terminated, before user logged in."),
    ok;


terminate(_Reason, State) ->
    lager:info("Session was terminated"),
    case wall_users:exist(State#state.username) of
        true ->  lager:info("removing the user ~tp", [State#state.username]),
                 wall_users:del(State#state.username);
        false -> lager:info("No user exist. Safe termination"),
                 ok
    end.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

% Registers/reregisters user in the database
register_user(Username, Socket, Transport, FirstTime) ->
    Status = wall_users:reg(Username, self()),
    lager:info(case FirstTime of
        true  -> "New user registred with states ~tp";
        false -> "Re-registred with new status ~tp"
    end, [Status]),

    NewState = #state{auth_status=true, username=Username, socket=Socket, transport=Transport},
    lager:info("returning new state ~tp", [NewState]),

    % Message that tells wtherer authentication is successful
    Transport:send(Socket, _AuthSucess = <<"OK">>),
    {noreply, NewState}.



%% TODO: send user name together with the message
%% That's why I decode ane encode message on the server side!
%% Sends broadcast message to all available clients
notify_other_clients(Username, Message) when is_list(Message) ->
    ActiveClients = wall_users:active_connections_except(self()),
    lager:info("Currently active clients are ~tp", [ActiveClients]),
    lager:info("Broadcasting: ~tp: ~tp", [Username, Message]),
    broadcast_message(ActiveClients, Username, encode_message(Message)).



%% Sends a farewell letter to the user,
%% removes they from database and drops the connection
notify_and_drop_given_client(Username) ->
    [{Username, {Pid, _Timestamp}}] = wall_users:find(Username),
    Message = encode_message(
        "A new user with following nickname connected. You will be dropped\n"),

    lager:info("Removing the user ~tp", [Username]),
    Status = wall_users:del(Username),
    lager:info("Removal status ~tp", [Status]),

    % Sends the final message and drops the user
    broadcast_message([Pid], Username, Message),
    Pid ! stop.



%% @doc Broadcast given message to other users
-spec broadcast_message (list(pid()), string(), binary()) -> ok.
broadcast_message([], _Username, _Message) ->
    ok;
broadcast_message([Pid|Pids], Username, Message) ->
    Pid ! {broadcast, Username, Message},
    broadcast_message(Pids, Username, Message).


%% @doc Encode message to the protocol-friendly format
-spec encode_message(string()) -> binary().
encode_message(Message) ->
    Payload     = term_to_binary(Message),
    PayloadSize = byte_size(Payload),

    Header = case binary:encode_unsigned(PayloadSize, big) of
                Byte  when byte_size(Byte)  =:=1 -> <<0, 0, Byte/bits>>;
                Bytes when byte_size(Bytes) =:=2 -> <<0, Bytes/bits>>;
                Bytes when byte_size(Bytes) =:=3 -> Bytes
             end,

    <<Header/binary, Payload/binary>>.


% @doc decodes the message and sends it to the user
decode_data(Data = <<Size:24/unsigned-big-integer, Rest/binary>>, State) ->
    case byte_size(Rest) >= Size of
         true  -> % At least one message was received
               <<MessageBody:Size/binary, BytesRem/binary>> = Rest,

               handle_message(MessageBody, State),

               % decode other data
               decode_data(BytesRem, State#state{buffer=BytesRem});
         false -> % No integral messages received
               % Put Data back into the buffer
               State#state{buffer=Data}
    end;
decode_data(Buffer, State) ->
    State#state{buffer=Buffer}.



% @doc Decodes the message and send it to the other clients
% @spec
-spec handle_message(binary(), state()) -> ok.
handle_message(MessageBody, State) ->
    % Decode the message
    DecodedMessage = deserialize(MessageBody),
    notify_other_clients(State#state.username, DecodedMessage),
    ok.


% @doc Deserializes the message's content
% @spec deserialize(binary()) -> term().
-spec deserialize(binary()) -> term().
deserialize(MessageBody) ->
    % throws an exception in case the presentce of atoms
    binary_to_term(MessageBody, [safe]).


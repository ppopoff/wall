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


%-record(state, {auth_status, socket, transport}).

-define(AUTH_HEADER, 16).
-define(SERVER, ?MODULE).
-define(TIMEOUT, 86400000).



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
    gen_server:enter_loop(?MODULE, [], {false, <<>>, Socket, Transport}, ?TIMEOUT).


%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------


%% This code handles authorisation
handle_info({tcp, Socket, Data}, State={false, _Uname, Socket, Transport}) ->
    lager:info("Authenticating the user"),
    Transport:setopts(Socket, [{active, once}]),

    %% About the authentication protocol:
    %% user sends A message (username)
    %% Message format:
    %% Header: 2 bytes big endian int (size of payload)
    %% Payload
    %% Client should get the following line in response
    %% 'OK'
    %%
    HeaderSize = 2,
    <<Header:HeaderSize/binary, Rest/binary>> = Data,


    MessageLength = binary:decode_unsigned(Header),
    lager:info("Header is ~tp", [MessageLength]),


    <<Name:MessageLength/binary, _Left/binary>> = Rest,
    lager:info("Name is: ~tp", [Name]),


    AuthSucess = <<"OK">>,

    lager:info("User with name ~tp is trying to authenticate", [Name]),
    lager:info("Checking username ~tp in the database", [Name]),

    case wall_users:exist(Name) of
        true ->
            lager:info("The specified user exist. Disconnecting..."),
            {stop, normal, State};
        false ->
            lager:info("no such user exist. Creating..."),
            % Registing the user with current process id
            wall_users:reg(Name, self()),
            NewState = {true, Name, Socket, Transport},
            lager:info("returning new state ~tp", [NewState]),
            Transport:send(Socket, AuthSucess),
            {noreply, NewState}
    end;


% SENDING
% When user is authenticated
% sends message to all the connected clients
handle_info({tcp, Socket, Data}, State={true, Username, Socket, Transport}) ->
    Transport:setopts(Socket, [{active, once}]),
    notify_other_clients(Username, Data),
    {noreply, State, ?TIMEOUT};


% RECEIVING
% Message broadcasting
handle_info({broadcast, _Username, Message}, State={true, _CurrName, Socket, Transport}) ->
    Transport:setopts(Socket, [{active, once}]),

    % todo may be add username here
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

handle_cast(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, _State={true, Username, _Socket, _Transport}) ->
    lager:info("Session was terminated"),
    case wall_users:exist(Username) of
        true ->  lager:info("removing the user ~tp", [Username]),
                 wall_users:del(Username);
        false -> lager:info("No user exist. Safe termination"),
                 ok
    end;


terminate(_Reason, _State={false, <<>>, _Socket, _Transport}) ->
    lager:info("Session was terminated, before user logged in."),
    ok.



code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

notify_other_clients(Username, Message) ->
    broadcast_message(
        wall_users:active_connections_except(self()),
        Username, Message
    ).


notify_given_client(Username, Message) ->
    {_, Pid, _} = wall_users:find(Username),
    Message = encode_message(
        "A new user with following nickname connected. You will be dropped"),

    Pid ! stop,
    broadcast_message([Pid], Username, Message).



broadcast_message([], _Username, _Message) ->
    ok;
broadcast_message([Pid|Pids], Username, Message) ->
    Pid ! {broadcast, Username, Message},
    broadcast_message(Pids, Username, Message).


%% message decoder/encoder
%%
-spec encode_message(string()) -> binary().
encode_message(Message) ->
    Payload = term_to_binary(Message),
    PayloadSize = byte_size(Payload),

    Header = case binary:encode_unsigned(PayloadSize, big) of
                Byte  when byte_size(Byte)  =:=1 -> <<0, 0, Byte/bits>>;
                Bytes when byte_size(Bytes) =:=2 -> <<0, Bytes/bits>>;
                Bytes when byte_size(Bytes) =:=3 -> Bytes
             end,

    <<Header/binary, Payload/binary>>.




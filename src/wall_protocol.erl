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
handle_info({tcp, Socket, Data}, State={false, _Username, Socket, Transport}) ->
    Transport:setopts(Socket, [{active, once}]),


    %% About the authentication protocol:
    %% user sends A message (username) that should be terminated with
    %% the following sequence '\r\n'
    %% Then server shoud authenticate the user and return
    %% 'OK\n'
    %% The prorocol was made that way because I wan't to be able to test
    %% using telnet
    [Name, _Rest] = binary:split(Data, <<"\r\n">>),
    AuthSucess = <<"OK\n">>,

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
handle_info({broadcast, Username, Message}, State={true, _CurrName, Socket, Transport}) ->
    Transport:setopts(Socket, [{active, once}]),

    % About the protocol:
    % It should send a number of messages
    % Header consists of two parts: message length
    % And name length
    %PayloadLength = byte_size(Message),
    %NameLength    = byte_size(Username),
    %Message = <<PayloadLength:3/unsigned-big-integer,
    %            NameLength:2/unsigned-big-integer,
    %            Username/binary,
    %            Message/binary>>,

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

terminate(_Reason, State={true, Username, _Socket, _Transport}) ->
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


broadcast_message([], _Username, _Message) ->
    ok;
broadcast_message([Pid|Pids], Username, Message) ->
    Pid ! {broadcast, Username, Message},
    broadcast_message(Pids, Username, Message).


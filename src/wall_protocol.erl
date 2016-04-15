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

-record(state, {socket, transport}).

-define(AUTH_HEADER, 16).
-define(SERVER, ?MODULE).
% TODO: Try to find a way to disable timeout
-define(TIMEOUT, 86400000).



start_link(Ref, Socket, Transport, Opts) ->
    proc_lib:start_link(?MODULE, init, [Ref, Socket, Transport, Opts]).

init([]) -> {ok, undefined}.

stop() ->
    lager:info("Stopping the listener").
%    gen_server:cast(?SERVER, stop).



init(Ref, Socket, Transport, _Opts = []) ->
    ok = proc_lib:init_ack({ok, self()}),
    ok = ranch:accept_ack(Ref),
    ok = Transport:setopts(Socket, [{active, once}]),
    gen_server:enter_loop(?MODULE, [],
        #state{socket=Socket, transport=Transport},
        ?TIMEOUT).


%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

%handle_info(timeout, _State=#state{ref=Ref, socket=Socket, transport=Transport}) ->
%  ranch:accept_ack(Ref),
%  Transport:setopts(Socket, [{active, once}]);

  %auth(State);

%% When message from client is received:
handle_info({tcp, Socket, Data}, State=#state{socket=Socket, transport=Transport}) ->
    Transport:setopts(Socket, [{active, once}]),
    Transport:send(Socket, <<"OK\n">>),
    {noreply, State, ?TIMEOUT};


% Message broadcasting
%handle_info({broadcast, Message}, State) ->
%    lager:info("Broadcasting message ~tp", Message),
%    {noreply, State};


handle_info({tcp_closed, _Socket}, State) ->
    {stop, normal, State};

handle_info({tcp_error, _, Reason}, State) ->
    {stop, Reason, State};

handle_info(timeout, State) ->
    {stop, normal, State};

handle_info(_Info, State) ->
    {stop, normal, State}.


handle_call(_Request, _From, State) ->
    {reply, ok, State}.


handle_cast(_Msg, State) ->
    {noreply, State}.


terminate(_Reason, _State) ->
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------



%% authentication protocol
%% first 2 bytes unsigned big endian integer length encoded
%% username in bytes. Following N bytes are the username in
%% erlang searilization format
%auth(State=#state{ref=Ref, socket=Socket, transport=Transport}) ->
%    ranch:accept_ack(Ref),

    % Prompt
%    Transport:send(Socket, <<"auth\n">>),

    % waiting for username
    %ranch:accept_ack(Ref),

%    AuthenticationDelay = 30000, % 30 seconds to authenticate

%    case Transport:recv(Socket, 0, AuthenticationDelay) of
%        {ok, Data} ->
%            [Name, Rest] = binary:split(Data, <<"\r\n">>),
%            CurrentProcessId = self(),
%
%            lager:info("User with name ~tp is trying to authenticate", [Name]),
%            lager:info("Checking username ~tp in the database", [Name]),
%            case wall_users:exist(Name) of
%                true ->
%                    lager:info("the following user exist"),
%                    {stop, normal, State};
%                false ->
%                    lager:info("no such user exist. Creating"),
%                    wall_users:reg(Name, CurrentProcessId),
%                    {noreply, State}
%            end;
%
%        {error, _} ->
%            lager:error("bad authentication data... Connection will be dropped"),
%            {stop, normal, State}
%    end.
%


%auth(Ref, Socket, Transport, <<Size:?AUTH_HEADER/unsigned-big-integer, Rest/bits>>) ->
%    <<UserNameBin:Size/binary>> = Rest,
%    UserName = binary_to_term(UserNameBin),
%    lager:info("Auth request from user: ~tp", [UserName]),
%    case wall_users:exist(UserName) of
%        true  ->
%            lager:info("User doesnt exist... Registration"),
            %wall_users:reg(UserName),
%            lager:info("authenticated ~tp", [UserName]),
%            Transport:send(Socket, <<"granted">>),
%            gen_server:enter_loop(
%                ?MODULE, [], #state{ref=Ref, socket=Socket, transport=Transport}, ?TIMEOUT);
%        false ->
%            lager:info("authentication failed"),
%            error
%    end.



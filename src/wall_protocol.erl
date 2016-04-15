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
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-record(state, {ref, socket, transport}).

-define(AUTH_HEADER, 16).
-define(SERVER, ?MODULE).
% Unlimited timeout
-define(TIMEOUT, 0).



start_link(Ref, Socket, Transport, Opts) ->
    gen_server:start_link(?MODULE, [Ref, Socket, Transport, Opts], []).


stop() ->
    lager:info("Stopping the listener"),
    gen_server:cast(?SERVER, stop).


init([Ref, Socket, Transport, _Opts]) ->
    {ok, #state{ref=Ref, socket=Socket, transport=Transport}, ?TIMEOUT}.


%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

handle_info(timeout, State) -> auth(State);

%% When message from client is received:
handle_info({tcp, Socket, Data}, State=#state{ref=_Ref, socket=Socket, transport=Transport}) ->
    Transport:setopts(Socket, [{active, once}]),
    Transport:send(Socket, Data),
    %% TODO : return something usefull
    %%io:format("~tp", [Data]),
    %%Transport:send(Socket, <<"OK\n">>),
    {noreply, State};

handle_info({tcp_closed, _Socket}, State) ->
    {stop, normal, State};

handle_info({tcp_error, _, Reason}, State) ->
    {stop, Reason, State};

handle_info(timeout, State) ->
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
auth(State=#state{ref=Ref, socket=Socket, transport=Transport}) ->
    ranch:accept_ack(Ref),

    % Prompt
    Transport:send(Socket, <<"auth\n">>),

    % waiting for username
    %ranch:accept_ack(Ref),

    AuthenticationDelay = 30000, % 30 seconds to authenticate

    case Transport:recv(Socket, 0, AuthenticationDelay) of
        {ok, Data} ->
            [Name, Rest] = binary:split(Data, <<"\r\n">>),
            NameAtom = list_to_atom(binary_to_list(Name)),

            lager:info("User with name ~tp is trying to authenticate", [NameAtom]),
            lager:info("Checking username ~tp in the database", [NameAtom]),
            case wall_users:exist(NameAtom) of
                true ->
                    lager:info("the following user exist"),
                    {stop, normal, State};
                false ->
                    lager:info("no such user exist. Creating"),
                    wall_users:reg(NameAtom),
                    {noreply, State}
            end;

        {error, _} ->
            lager:error("bad authentication data... Connection will be dropped"),
            {stop, normal, State}
    end.



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



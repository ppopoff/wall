-module(wall_protocol).
-behaviour(gen_server).
-behaviour(ranch_protocol).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/4]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1,
         init/4
         handle_call/3
         handle_cast/2
         handle_info/2
         terminate/2
         code_change/3
       ]).


-define(TIMEOUT, 99999).

-record(state, {socket, transport}).


start_link(Ref, Socket, Transport, Opts) ->
    proc_lib:start_link(?MODULE, init, [Ref, Socket, Transport, Opts]).
    %% Plain old and working version
    %%Pid = spawn_link(?MODULE, init, [Ref, Socket, Transport, Opts]),
    %%{ok, Pid}.


init(Ref, Socket, Transport, _Opts = []) ->
    ok = proc_lib:init_ack({ok, self()}),
    ok = ranch:accept_ack(Ref),
    ok = Transport:setopts(Socket, [{active, once}]),
    gen_server:enter_loop(?MODULE, [], #state{socket=Socket, transport=Transport},?TIMEOUT).
    %% Plain old and working version
    %ok = ranch:accept_ack(Ref),
    %loop(Socket, Transport).


%loop(Socket, Transport) ->
%    case Transport:recv(Socket, 0, ?TIMEOUT) of
%        {ok, Data} ->
%            lager:info("echoing ~p", [Data]),
%            Transport:send(Socket, Data),
%            loop(Socket, Transport);
%        _ ->
%            ok = Transport:close(Socket)
%    end.


%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

handle_info({tcp, Socket, Data}, State=#state{socket=Socket, transport=Transport}) ->
    Transport:setopts(Socket, [{active, once}]),
    Transport:send(Socket, wall_message_transformer:decode_message(Data)),
    {noreply, State, ?TIMEOUT};
handle_info({tcp_closed, _Socket}, State) ->
    {stop, normal, State};
handle_info({tcp_error, _, Reason}, State) ->
    {stop, Reason, State};
handle_info(timeout, State) ->
    {stop, normal, State};
handle_info(_Info, State) ->
    {stop, normal, State}.


%% This function wont be called
%% it's here to satisfy gen_server behaviour
init([]) ->
    {ok, unit}.

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


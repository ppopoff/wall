-module(wall_protocol).
%-behaviour(gen_server).
-behaviour(ranch_protocol).

% API
-export([start_link/4]).

% gen_server
%-export([init/1]).
-export([init/4]).
%-export([handle_call/3]).
%-export([handle_cast/2]).
%-export([init/4]).
%-export([init/4]).
%-export([init/4]).
%-export([init/4]).


-define(TIMEOUT, 99999).


-record(state, {socket, transport}).



%% API

start_link(Ref, Socket, Transport, Opts) ->
    Pid = spawn_link(?MODULE, init, [Ref, Socket, Transport, Opts]),
    {ok, Pid}.


init(Ref, Socket, Transport, _Opts = []) ->
    ok = ranch:accept_ack(Ref),
    loop(Socket, Transport).


loop(Socket, Transport) ->
    case Transport:recv(Socket, 0, ?TIMEOUT) of
        {ok, Data} ->
            lager:info("echoing ~p", [Data]),
            Transport:send(Socket, Data),
            loop(Socket, Transport);
        _ ->
            ok = Transport:close(Socket)
    end.

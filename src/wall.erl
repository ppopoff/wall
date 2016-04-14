-module(wall).

-behaviour(application).

%% Application callbacks
-export([start/2, shutdown/0, stop/1]).



%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    lager:info("Starting the ranch listener"),

    NumberOfAcceptors = 1,
    Port = 8000,
    Protocol = wall_protocol,

    %% Pay attention to [{active, once}]
    {ok, _} = ranch:start_listener(
        wall_tcp, NumberOfAcceptors, ranch_tcp, [{port, Port}], Protocol, []),

    case wall_sup:start_link() of
        {ok, Pid} -> {ok, Pid};
        Error     -> lager:error("Unable to start supervisor"),
                     Error
    end.

stop(_State) ->
    lager:info("Shutting down the application~n"),
    ok.

shutdown() ->
    application:stop(wall).

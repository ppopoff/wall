-module(wall).

-behaviour(application).

%% Application callbacks
-export([start/2, shutdown/0, stop/1]).


-define(LOG, lager).


%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, StartArgs) ->
    ?LOG:info("Starting the ranch listener"),

    %% TODO: eliminate port hard-coding (use erlang configs instead)
    ranch:start_listener(
        wall, 1, ranch_tcp, [{port, 1337}], wall_protocol, []),

    ?LOG:info("Starting the application supervisor"),
    case wall_sup:start_link(StartArgs) of
        {ok, Pid} -> {ok, Pid};
        Error     -> Error
    end.

stop(_State) ->
    ?LOG:info("Shutting down the application~n"),
    ok.

shutdown() ->
    application:stop(wall).

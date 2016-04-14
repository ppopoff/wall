-module(wall).

-behaviour(application).

%% Application callbacks
-export([start/2]).
-export([run/0]).
-export([shutdown/0]).
-export([stop/1]).



%% ===================================================================
%% Application callbacks
%% ===================================================================
%%
run() ->
    application:ensure_all_started(ranch),
    application:ensure_all_started(compiler),
    application:ensure_all_started(syntax_tools),
    application:ensure_all_started(goldrush),
    application:ensure_all_started(lager),
    application:ensure_all_started(wall),
    ok.


start(_StartType, _StartArgs) ->
    lager:info("Starting the ranch listener"),

    case wall_sup:start_link() of
        {ok, Pid} -> lager:info("Supervisor started ~tp", Pid);
        Error     -> lager:error("Unable to start supervisor"),
                     Error
    end,

    NumberOfAcceptors = 1,
    Port = 8000,
    Protocol = wall_protocol,
    Options = [{port, Port}, {max_connections, infinity}],

    {ok, _} = ranch:start_listener(
        wall_tcp, NumberOfAcceptors, ranch_tcp, Options, Protocol, []).


stop(_State) ->
    lager:info("Stopping the ranch listener"),
    ranch:stop_listener(wall_tcp),
    lager:info("Shutting down the application"),
    ok.

shutdown() ->
    application:stop(wall).

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
%% @doc ensures that all previous applications were started
run() ->
    application:ensure_all_started(wall),
    ok.


%% @doc starts the appication
start(_StartType, _StartArgs) ->
    lager:info("Starting the ranch listener"),

    _ProcessId = case wall_sup:start_link() of
        {ok, Pid} -> lager:info("Supervisor started with pid: ~tp", [Pid]),
                     Pid;
        Error     -> lager:error("Unable to start supervisor"),
                     Error
    end,

    lager:info("Application details:"),
    NumberOfAcceptors = get_app_env(acceptorsNum, 10),
    lager:info("Number of acceptors: ~tp", [NumberOfAcceptors]),
    Port = get_app_env(port),
    lager:info("Port number: ~tp", [Port]),
    Protocol = wall_protocol,
    Options = [{port, Port}],

    {ok, _RanchPid} = ranch:start_listener(
        wall_tcp, NumberOfAcceptors, ranch_tcp, Options, Protocol, []).


%% @doc stops the application
-spec stop(any()) -> ok.
stop(_State) ->
    lager:info("Stopping the ranch listener"),
    ranch:stop_listener(wall_tcp),
    lager:info("Shutting down the application"),
    ok.


%% @doc stops the current application
-spec shutdown() -> ok | {error, any()}.
shutdown() ->
    application:stop(wall).


%% @private
%% @doc return a config value
get_app_env(Key) ->
    get_app_env(Key, undefined).


%% @private
%% @doc return a config value
get_app_env(Key, DefaultValue) ->
    case application:get_env(wall, Key) of
        {ok, Value} -> Value;
        undefined   -> DefaultValue
    end.

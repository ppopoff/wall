-module(wall_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).


%% ===================================================================
%% API functions
%% ===================================================================

-spec start_link() -> {ok, pid()}.
start_link() ->
    lager:info("Supervisor started"),
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    %% means that for each process that dies simply restart that
    %% process without affecting any of the others
    RestartStrategy        = one_for_one,
    MaxRestarts            = 10,
    MaxTimeBetweenRestarts = 10,

    SupervisorFlags = {
        RestartStrategy, MaxRestarts, MaxTimeBetweenRestarts},

    {ok, {SupervisorFlags, []}}.


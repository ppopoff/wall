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
        RestartStrategy, MaxRestarts, MaxTimeBetweenRestarts
    },

    ChildSpec = [
       {
         storage, % Internal name *used only by supervisor
         {wall_users, start_link, []}, % function: M/F/A
         transient, % permanent | temporary
         infinity,
         worker, % can be worker | supervisor
         dynamic
       }
    ],

    {ok, {SupervisorFlags, ChildSpec}}.


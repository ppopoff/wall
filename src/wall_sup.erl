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
    Procs = [
            {storage, {wall_users, start_link, []},
                transient, infinity, worker, dynamic}
    ],
    {ok, {{one_for_one, 1, 10}, Procs}}.


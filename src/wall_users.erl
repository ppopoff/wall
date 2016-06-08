-module(wall_users).
-behaviour(gen_server).

-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]).
-export([stop/0]).
-export([reg/2]).
-export([rreg/2]).
-export([del/1]).
-export([exist/1]).
-export([find/1]).
-export([active_connections/0]).
-export([active_connections_except/1]).
-export([stat/0]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).


-record(stats, {
    users  :: list(pid()),
    length :: integer()
}).

-type user()      :: {binary(), pid(), integer()}.
-type username()  :: binary().
-type stats()     :: #stats{}.
-type tab()       :: atom() | any().


%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

%% @doc Creates an instance of gen_server
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).


%% @doc Stops the server
stop() ->
    gen_server:cast(?SERVER, stop).


%% @doc Adds user to the ets table
-spec reg(Username :: binary(), Pid :: pid()) -> ok.
reg(Username, Pid) ->
    gen_server:call(?SERVER, {reguser, Username, Pid}).


%% @doc Updates the user's pid in ets table
-spec rreg(Username :: username(), Pid :: pid()) -> ok.
rreg(Username, Pid) ->
    gen_server:call(?SERVER, {rereguser, Username, Pid}).


%% @doc Removes user from the ets table
-spec del(username()) -> boolean().
del(Username) ->
    gen_server:call(?SERVER, {deluser, Username}).


%% @doc Checks whether user exist, if so
-spec exist(username()) -> boolean().
exist(Username) ->
    gen_server:call(?SERVER, {is_registered, Username}).


%% @doc Searches for user in the ets
-spec find(username()) -> [user()].
find(Username) ->
    gen_server:call(?SERVER, {find, Username}).


%% @doc Returns list of all active connections
-spec active_connections() -> [pid()].
active_connections() ->
    gen_server:call(?SERVER, connections).


%% @doc Returns list of active connections (PIDs), except given one
-spec active_connections_except(pid()) -> [pid()].
active_connections_except(Pid) ->
    gen_server:call(?SERVER, {connections_except, Pid}).


%% Returns the statistic information about the
%% number of users and other details
stat() ->
    gen_server:call(?SERVER, stat).


%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

%% @doc initializes the internal ets storage
init([]) ->
    TableId = ets:new(storage, [private, set]),
    {ok, TableId}.


%% @doc Cleans up the resorces
-spec terminate(Reason :: any(), TableId :: tab()) -> ok.
terminate(Reason, TableId) ->
    Status =  ets:delete(TableId),
    ok.


%% @hidden This feature is not supported
handle_info(_Info, TableId) ->
    {noreply, TableId}.


%% @hidden feature is not supported
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% TODO: add specs
handle_call({is_registered, Username}, _From, TableId) ->
    {reply, is_registered(TableId, Username), TableId};
handle_call(connections, _From, TableId) ->
    {reply, get_active_connections(TableId), TableId};
handle_call({connections_except, Pid}, _From, TableId) ->
    {reply, get_active_connections_except(TableId, Pid), TableId};
handle_call({reguser, Username, Pid}, _From, TableId) ->
    {reply, register_user(TableId, Username, Pid), TableId};
handle_call({rereguser, Username, Pid}, _From, TableId) ->
    {reply, reregister_user(TableId, Username, Pid), TableId};
handle_call({deluser, Username}, _From, TableId) ->
    {reply, delete_user(TableId, Username), TableId};
handle_call({find, Username}, _From, TableId) ->
    {reply, find_user(TableId, Username), TableId};
handle_call(stat, _From, TableId) ->
    {reply, get_statistics(TableId), TableId}.


%% @doc Stops the server
handle_cast(stop, TableId) ->
    {stop, normal, TableId}.


%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

%% @doc Defines whether user is registred by results of lookup function
-spec is_registered(TableId :: tab(), Username :: username()) -> boolean().
is_registered(TableId, Username) ->
    case ets:lookup(TableId, Username) of
        []  -> false;
        [_] -> true
    end.


%% @doc Registers user with given name
-spec register_user(TableId :: tab(), Username :: binary(), Pid :: pid()) -> ok.
register_user(TableId, Username, Pid) ->
    Status = ets:insert(TableId, {Username, {Pid, get_registration_date()}}),
    ok.


%% @doc Registers user with given name
-spec reregister_user(TableId :: tab(), Username :: binary(), NewPid :: pid()) -> ok.
reregister_user(TableId, Username, NewPid) ->
    delete_user(TableId, Username),
    Status = ets:insert_new(TableId, {Username, {NewPid, get_registration_date()}}),
    ok.


%% @doc Removes registered user
-spec delete_user(TableId :: tab(), Username :: username()) -> ok.
delete_user(TableId, Username) ->
    case ets:delete(TableId, Username) of
        true ->  lager:info("User ~tp was successfully removed", [Username]),
                 ok;
        false -> lager:warning("User ~tp was not removed", [Username]),
                 ok
    end.


%% @doc Returns name and registration date
-spec find_user(TableId :: tab(), Username :: username()) -> [user()].
find_user(TableId, Username) ->
    ets:lookup(TableId, Username).


%% @doc Retruns a map with user statistics
-spec get_statistics(TableId :: tab()) -> stats().
get_statistics(TableId) ->
    Users = ets:tab2list(TableId),
    #stats{length=length(Users), users=Users}.


%% @doc retrieves a list of active connections without a given one
-spec get_active_connections_except(TableId :: tab(), PidToExclued :: pid()) -> list(pid()).
get_active_connections_except(TableId, PidToExclude) ->
    lists:filter(
        fun(Pid) -> Pid =/= PidToExclude end,
        get_active_connections(TableId)
     ).


%% @doc Returns list of all active connections
-spec get_active_connections(TableId :: tab()) -> list(pid()).
get_active_connections(TableId) ->
    lager:info("Retrieving the list of active connections"),
    Data = ets:tab2list(TableId),
    Pids = lists:map(fun ({_Username, {Pid, _RegDate}}) -> Pid end, Data),
    Pids.


%% @doc Retrieves the registration date
-spec get_registration_date() -> integer().
get_registration_date() ->
    get_time_millis(os:timestamp()).


%% @doc Returns current time (since epoch) in milliseconds
-spec get_time_millis(Now :: {integer(), integer(), integer()}) -> integer().
get_time_millis(Now) ->
    {Mega, Sec, Micro} = Now,
    (Mega * 1000000 + Sec) * 1000000 + Micro.


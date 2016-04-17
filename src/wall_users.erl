-module(wall_users).
-behaviour(gen_server).

-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]).
-export([stop/0]).
-export([reg/2]).
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


%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).


% Stops the server
stop() ->
    gen_server:cast(?SERVER, stop).


% Adds user to the ets table
reg(UserName, Pid) when is_binary(UserName) ->
    gen_server:call(?SERVER, {reguser, UserName, Pid}).


% Removes user from the ets table
-spec del(binary()) -> boolean().
del(UserName) when is_binary(UserName) ->
    gen_server:call(?SERVER, {deluser, UserName}).


% Checks whether user exist, if so
% returns true, false otherwise
-spec exist(binary()) -> boolean().
exist(UserName) when is_binary(UserName)->
    gen_server:call(?SERVER, {'is registered', UserName}).


% Searches for user in the ets
-spec find(binary()) -> [{binary(), pid(), integer()}].
find(UserName) ->
    gen_server:call(?SERVER, {find, UserName}).


% Returns list of all active connections
-spec active_connections() -> [pid()].
active_connections() ->
    gen_server:call(?SERVER, connections).


% Returns list of active connections (PIDs), except
% for given one.
-spec active_connections_except(pid()) -> [pid()].
active_connections_except(Pid) ->
    gen_server:call(?SERVER, {connections_except, Pid}).


% Returns the statistic information about the
% number of users and other details
stat() ->
    gen_server:call(?SERVER, stat).


%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([]) ->
    lager:info("Initializing the user storage..."),
    lager:info("Creating a table"),
    TableId = ets:new(storage, [private, set]),
    lager:info("Creating a table"),
    lager:info("this is my table_id: ~tp", [TableId]),
    {ok, TableId}.


terminate(Reason, TableId) ->
    lager:info("Stopping the table service"),
    lager:info("Deleting the ~tp table", [TableId]),
    Status =  ets:delete(TableId),
    lager:info("Done! ~tp", [Status]),
    lager:info("Terminated by following reason: ~tp~n", [Reason]),
    ok.


%% This feature is not supported
handle_info(_Info, TableId) ->
    {noreply, TableId}.


%% This feature is not supported
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


handle_call({'is registered', Username}, _From, TableId) ->
   {reply, is_registered(ets:lookup(TableId, Username)), TableId};
handle_call(connections, _From, TableId) ->
   {reply, get_active_connections(TableId), TableId};
handle_call({connections_except, Pid}, _From, TableId) ->
   {reply, get_active_connections_except(TableId, Pid), TableId};
handle_call({reguser, Username, Pid}, _From, TableId) ->
   {reply, register_user(TableId, Username, Pid), TableId};
handle_call({deluser, Username}, _From, TableId) ->
   {reply, delete_user(TableId, Username), TableId};
handle_call({find, Username}, _From, TableId) ->
    {reply, find_user(TableId, Username), TableId};
handle_call(stat, _From, TableId) ->
    {reply, get_statistics(TableId), TableId}.


%% Stops the server
handle_cast(stop, TableId) ->
    {stop, normal, TableId}.


%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

% Defines whether user is registred by
% results of lookup function
is_registered([])    -> false;
is_registered([_])   -> true.


% Registers user with given name
register_user(TableId, UserName, Pid) ->
    lager:info("Registering a new user..."),
    RegistrationDate = get_time_millis(os:timestamp()),
    Status = ets:insert(TableId, {UserName, {Pid, RegistrationDate}}),
    lager:info("User registration status ~tp", [Status]),
    lager:info("User ~tp registered ~tp. Acceptor id: ~tp", [UserName, RegistrationDate, Pid]),
    {ok, UserName, Pid, RegistrationDate}.


% Removes registered user
delete_user(TableId, Username) ->
    lager:info("Removing user ~tp from database..", [Username]),
    case ets:delete(TableId, Username) of
        true ->  lager:info("User was successfully removed"),
                 ok;
        false -> lager:warning("User was not removed"),
                 ok
    end.


% Returns name and registration date
find_user(TableId, UserName) ->
    lager:info("Searching for user ~tp", [UserName]),
    ets:lookup(TableId, UserName).


% retruns a map with user statistics
get_statistics(TableId) ->
    lager:info("Statistics for the table"),
    Users = ets:tab2list(TableId),
    Length = length(Users),
    {Length, Users}.


% returns list with active connections
get_active_connections(TableId) ->
    lager:info("Retrieving the list of active connections"),
    Data = ets:tab2list(TableId),
    Pids = lists:map(fun ({_Uname, {Pid, _RegDate}}) -> Pid end, Data),
    Pids.


get_active_connections_except(TableId, PidToExclude) ->
    lists:filter(
        fun(Pid) -> Pid =/= PidToExclude end,
        get_active_connections(TableId)
     ).


% Returns current time (since epoch) in milliseconds
get_time_millis(Now) ->
    {Mega, Sec, Micro} = Now,
    (Mega * 1000000 + Sec) * 1000000 + Micro.

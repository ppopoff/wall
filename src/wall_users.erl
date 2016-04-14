-module(wall_users).
-behaviour(gen_server).

-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]).
-export([stop/0]).
-export([reg/1]).
-export([exist/1]).
-export([find/1]).

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


stop() ->
    gen_server:cast(?SERVER, stop).


reg(UserName) when is_atom(UserName) ->
    gen_server:call(?SERVER, {reguser, UserName}).

-spec exist(atom()) -> boolean().
exist(UserName) when is_atom(UserName) ->
    gen_server:call(?SERVER, {'is registered', UserName}).


-spec find(atom()) -> [{atom(), integer()}].
find(UserName) ->
    gen_server:call(?SERVER, {find, UserName}).


%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([]) ->
    lager:info("Initializing the user storage..."),
    lager:debug("Creating a table"),
    TableId = ets:new(storage, [private, set]),
    lager:debug("Creating a table"),
    lager:debug("this is my table_id: ~tp", [TableId]),
    {ok, TableId}.

terminate(Reason, TableId) ->
    lager:info("Stopping the table service"),
    lager:debug("Deleting the ~tp table", [TableId]),
    Status =  ets:delete(TableId),
    lager:debug("Done! ~tp", [Status]),
    lager:debug("Terminated by following reason: ~tp~n", [Reason]),
    ok.


%% This feature is not supported
handle_info(_Info, TableId) ->
    {noreply, TableId}.


%% This feature is not supported
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


handle_call({'is registered', UserName}, _From, TableId) ->
   {reply, is_registered(ets:lookup(TableId, UserName)), TableId};
handle_call({reguser, UserName}, _From, TableId) ->
   {reply, register_user(TableId, UserName), TableId};
handle_call({find, UserName}, _From, TableId) ->
    {reply, find_user(TableId, UserName), TableId}.


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
register_user(TableId, UserName) when is_atom(UserName) ->
    lager:info("Registering a new user..."),
    RegistrationDate = os:timestamp(),
    Status = ets:insert(TableId, {UserName, get_time_millis(RegistrationDate)}),
    lager:debug("User registration status ~tp", [Status]),
    lager:info("User ~tp registered at: ~tp", [Status, RegistrationDate]),
    {ok, UserName, RegistrationDate}.
 

% Returns name and registration date
find_user(TableId, UserName) when is_atom(UserName) ->
    lager:debug("Searching for user ~tp", [UserName]),
    ets:lookup(TableId, UserName).

% Returns current time (since epoch) in milliseconds
get_time_millis(Now) ->
    {Mega, Sec, Micro} = Now,
    (Mega * 1000000 + Sec) * 1000000 + Micro.

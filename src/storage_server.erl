-module(storage_server).
-behaviour(gen_server).

-define(SERVER, ?MODULE).
-define(TABLE_ID, storage).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]).
-export([stop/2]).
-export([insert/2]).
-export([find/2]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/0, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

stop() ->
    gen_server:cast(?SERVER, stop).


insert(Key, Value) ->
    ets:insert(?TABLE_ID, {Key, Value}).


find(Key) ->
    ets:lookup(?TABLE_ID, Key).


get_table_as_list() ->
    ets:tab2list(?TABLE_ID).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------


init([]) ->
    % Possible modes are: set, ordered_set, bag, duplicate_bag
    % no duplicates are allowed
    Mode = set,

    %% TODO: replace with lager
    io:format("Creating a table"),
    Table = ets:new(?TABLE_ID, [Mode],
    {ok, ?TABLE_ID}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(Reason, _State) ->
    %% TODO: replace with lager
    io:format("Stopping the table service~n"),
    ets:delete(?TABLE_ID),

    %% TODO: replace with lager
    io:format("Terminated by following reason: ~tp~n", [Reason]),
    ok.

%% This feature is not supported
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------


%% Stops the server
handle_cast(stop, _State) ->
    {stop, normal, _State).



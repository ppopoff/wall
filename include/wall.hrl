%%% Definitions of the most used
%%% macroses and aliases
-author(ppopoff).

% Message map fields
-define(TIMESTAMP_FIELD, "t").
-define(MESSAGE_FIELD,   "m").
-define(USER_FIELD,      "u").


-define(SERVER, ?MODULE).

%% Heades size must be 3 bytes long
-define(HEADER_SIZE, 24).

%% Default timeout value (for not it's hardcoded)
-define(TIMEOUT, infinity).


%% Records and types

-type message()   :: map().
-type username()  :: binary().
-type transport() :: any().


% Connection state
-record(state, {
    auth_status = false :: boolean(),
    was_dropped = false :: boolean(),
    username = <<>>     :: username(),
    socket              :: port(),
    transport           :: transport(),
    buffer = <<>>       :: binary()
}).

-type state() :: #state{}.



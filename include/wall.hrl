%%% Definitions of the most used
%%% macroses and aliases
-author(ppopoff).

% Message map fields
-define(TIMESTAMP_FIELD, <<"t">>).
-define(MESSAGE_FIELD,   <<"m">>).
-define(USER_FIELD,      <<"u">>).

-define(AUTH_REQ,    <<"auth">>).
-define(AUTH_REQ_S,  "auth").
-define(AUTH_RES,    <<"ok">>).
-define(FROM_SERVER, <<"server">>).

%% Heades size must be 3 bytes long
-define(HEADER_SIZE, 24).
-define(BYTE, 8).

%% Default timeout value (for not it's hardcoded)
-define(TIMEOUT, infinity).


%% Records and types

-type message()   :: map().
-type username()  :: binary().
-type transport() :: any().



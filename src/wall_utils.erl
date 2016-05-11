%%% Contains various utils that perform message
%%% construction serialization/deserialization

-module(wall_utils).
-author(ppopoff).
-include("wall.hrl").
-export([deserialize/1]).
-export([add_timestamp/1]).
-export([message/2]).
-export([append_newline/1]).
-export([auth_success/0]).


%% @doc Deserializes the message's content
%% Trows an exception if atoms are present
-spec deserialize(MessageBody :: message()) -> term().
deserialize(MessageBody) ->
    try binary_to_term(MessageBody, [safe]) of
        Message   -> {ok, Message}
    catch
        Exception -> {failed, Exception}
    end.


%% @doc Adds server timestamp to the given message
-spec add_timestamp(Message :: message()) -> message().
add_timestamp(Message) ->
    Now = os:timestamp(),
    LocalTime = calendar:now_to_local_time(Now),
    maps:put(?TIMESTAMP_FIELD, LocalTime, Message).


%% @doc constructor for the message
-spec message(Username :: binary(), Message :: binary()) -> binary()
    ; message(Username :: string(), Message :: string()) -> binary().
message(Username, Message) when is_binary(Username) andalso is_binary(Message) ->
    wall_codec:encode_message(
        add_timestamp(#{
            ?MESSAGE_FIELD => append_newline(Message),
            ?USER_FIELD    => Username
    }));
message(Username, Message) when is_list(Username) andalso is_list(Message) ->
    message(list_to_binary(Username), list_to_binary(Message)).


%% @doc Creates a response that will be sent in case of successful
%% authentication
-spec auth_success() -> binary().
auth_success() ->
    message(?FROM_SERVER, ?AUTH_RES).


%% @doc Adds newline char at the end of the string if needed
-spec append_newline(String :: binary()) -> binary().
append_newline(String) ->
    case (binary:last(String)) of
       <<"\n">> -> String;
       _Char    -> << String/binary, <<"\n">>/binary >>
    end.


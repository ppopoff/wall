%%% Contains various utils that perform message
%%% construction serialization/deserialization

-module(wall_utils).
-author(ppopoff).
-export([deserialize/1]).
-export([add_timestamp/1]).
-export([message/2]).
-export([append_newline/1]).
-include("wall.hrl").


%% @doc Deserializes the message's content
%% Trows an exception if atoms are present
%% @spec deserialize(message()) -> term().
-spec deserialize(message()) -> term().
deserialize(MessageBody) ->
    binary_to_term(MessageBody, [safe]).


%% @doc Adds server timestamp to the given message
%% @spec add_timestamp(Message :: message()) -> message().
-spec add_timestamp(Message :: message()) -> message().
add_timestamp(Message) ->
    Now = os:timestamp(),
    LocalTime = calendar:now_to_local_time(Now),
    maps:put(?TIMESTAMP_FIELD, LocalTime, Message).


%% @doc constructor for the message
%% @spec message(Username :: string(), Message :: string()) -> binary().
-spec message(Username :: string(), Message :: string()) -> binary().
message(Username, Message) ->
    wall_codec:encode_message(
        add_timestamp(#{
            ?MESSAGE_FIELD => append_newline(Message),
            ?USER_FIELD    => Username
    })).


%% @doc Adds newline char at the end of the string if needed
%% @spec append_newline(string()) -> string().
-spec append_newline(String :: string()) -> string().
append_newline(String) ->
    case lists:last(String) of
       $\n -> String;
       _   -> String ++ "\n"
    end.


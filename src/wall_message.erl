%%% This module contains decoding and encoding
%%% functions, for the wall protocol
-module(wall_message).
-author(ppopoff).
-export([new/2, encode/1, encode/2, decode/1]).
-export([with_timestamp/1, auth_success/0]).

-export_type([username/0, message/0]).
-include("wall.hrl").


%% Records and types
-type username() :: binary().
-type message()  :: map().


%% @doc Encodes message to binary
-spec encode(message()) -> binary().
encode(Message) ->
    EncodedPayload = term_to_binary(Message),
    PayloadSize    = byte_size(EncodedPayload),
    <<PayloadSize:?HEADER_SIZE/unsigned-big-integer, EncodedPayload/bits>>.


%% @doc Creates the message and encodes it
-spec encode(string(), string()) -> binary().
encode(Username, Message) ->
    EncodedPayload  = term_to_binary(#{
        ?MESSAGE_FIELD => list_to_binary(Message),
        ?USER_FIELD    => list_to_binary(Username)
    }),
    PayloadSize = byte_size(EncodedPayload),
    <<PayloadSize:?HEADER_SIZE/unsigned-big-integer, EncodedPayload/bits>>.


%% @doc Deserializes the message's content
%% Trows an exception if atoms are present
-spec decode(binary()) -> term().
decode(MessageBody) ->
    try binary_to_term(MessageBody, [safe]) of
        Message   -> {ok, Message}
    catch
        Exception -> {failed, Exception}
    end.


%% @doc constructor for the message
-spec new(binary(), binary()) -> binary()
       ; (string(), string()) -> binary().
new(Username, Message) when is_binary(Username) andalso is_binary(Message) ->
    wall_message:encode(
        with_timestamp(#{
            ?MESSAGE_FIELD => append_newline(Message),
            ?USER_FIELD    => Username
    }));
new(Username, Message) when is_list(Username) andalso is_list(Message) ->
    new(list_to_binary(Username), list_to_binary(Message)).


%% @doc Creates a response that will be sent in case of successful
%% authentication
-spec auth_success() -> binary().
auth_success() ->
    new(?FROM_SERVER, ?AUTH_RES).


%% @doc @private Adds newline char at the end of the string if needed
-spec append_newline(binary()) -> binary().
append_newline(String) ->
    case (binary:last(String)) of
       <<"\n">> -> String;
       _Char    -> << String/binary, <<"\n">>/binary >>
    end.


%% @doc Adds server timestamp to the given message
-spec with_timestamp(map()) -> map().
with_timestamp(Message) ->
    Now = os:timestamp(),
    LocalTime = calendar:now_to_local_time(Now),
    maps:put(?TIMESTAMP_FIELD, LocalTime, Message).


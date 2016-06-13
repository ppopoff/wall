%%% This module contains decoding and encoding
%%% functions, for the wall protocol
-module(wall_message).
-author(ppopoff).
-export([encode/1, encode/2]).
-export_type([username/0, message/0]).
-include("wall.hrl").


%% Records and types
-type username()  :: binary().
-type message()   :: map().


%% @doc Encodes message to binary
-spec encode_message(message()) -> binary().
encode(Message) ->
    EncodedPayload = term_to_binary(Message),
    PayloadSize    = byte_size(EncodedPayload),
    <<PayloadSize:?HEADER_SIZE/unsigned-big-integer, EncodedPayload/bits>>.


%% @doc Creates the message and encodes it
-spec encode_message(string(), string()) -> binary().
encode(Username, Message) ->
    EncodedPayload  = term_to_binary(#{
        ?MESSAGE_FIELD => list_to_binary(Message),
        ?USER_FIELD    => list_to_binary(Username)
    }),
    PayloadSize = byte_size(EncodedPayload),
    <<PayloadSize:?HEADER_SIZE/unsigned-big-integer, EncodedPayload/bits>>.


%%% This module contains decoding and encoding
%%% functions, for the wall protocol
-module(wall_codec).
-author(ppopoff).
-export([encode_message/1, encode_message/2]).
-export_type([username/0, message/0]).
-include("wall.hrl").


%% Records and types
-type username()  :: binary().
-type message()   :: map().


%% @doc Encodes message to binary
-spec encode_message(Message :: message()) -> binary().
encode_message(Message) ->
    EncodedPayload = term_to_binary(Message),
    PayloadSize    = byte_size(EncodedPayload),
    <<PayloadSize:?HEADER_SIZE/unsigned-big-integer, EncodedPayload/bits>>.


%% @doc Creates the message and encodes it
-spec encode_message(Username :: string(), Message :: string()) -> binary().
encode_message(Username, Message) ->
    EncodedPayload  = term_to_binary(#{
        ?MESSAGE_FIELD => list_to_binary(Message),
        ?USER_FIELD    => list_to_binary(Username)
    }),
    PayloadSize = byte_size(EncodedPayload),
    <<PayloadSize:?HEADER_SIZE/unsigned-big-integer, EncodedPayload/bits>>.


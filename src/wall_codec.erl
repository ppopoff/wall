%%% This module contains decoding and encoding
%%% functions, for the wall protocol
-module(wall_codec).
-author(ppopoff).
-export([encode_message/1]).
-include("wall.hrl").


%% @doc Encodes message to binary
%% @spec encode_message(message()) -> binary().
-spec encode_message(Message :: message()) -> binary().
encode_message(Message) ->
    EncodedPayload = term_to_binary(Message),
    PayloadSize    = byte_size(EncodedPayload),
    <<PayloadSize:24/unsigned-big-integer, EncodedPayload/bits>>.


%% @doc Encodes the message
%% @spec encode_message(string(), string()) -> binary()
-spec encode_message(string(), string()) -> binary().
encode_message(Username, Message) ->
    EncodedPayload  = term_to_binary(#{"m" => Message, "u" => Username}),
    PayloadSize     = byte_size(EncodedPayload),
    <<PayloadSize:24/unsigned-big-integer, EncodedPayload/bits>>.



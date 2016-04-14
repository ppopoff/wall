-module(wall_message_transformer).

% API
-export([decode_message/1, encode_message/1]).

% 3 bytes header
-define(HEADER, 24).


%% Format specification
%% The first 3 bytes (unsigned-big-endian) -> package length
%% Others -> payload
%%
%% Payload should be encoded as External Term Format
%% http://erlang.org/doc/apps/erts/erl_ext_dist.html
%% Comment: use internal functions to decode the payload data

-spec decode_message(binary()) -> bitstring().
decode_message(<<PackageLength:?HEADER/unsigned-big-integer, Rest/binary>>)->
    EncodedMessage = <<Rest:PackageLength/binary>>,
    DecodedMessage = binary_to_term(EncodedMessage),
    DecodedMessage.



%% Endodes message payload using term_to_binary
%% format
-spec encode_message(term()) -> binary().
encode_message(Term) ->
    BinaryRepresentation = term_to_binary(Term),
    LengthInBytes = byte_size(BinaryRepresentation),
    <<LengthInBytes:?HEADER/unsigned-big-integer, BinaryRepresentation/binary>>.


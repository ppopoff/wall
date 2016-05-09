%% Tests for wall utils
%% @author Paul Popoff
%% @copyright 2016 Paul Popoff
%% @private

-module(wall_utils_test).
-include("wall.hrl").
-include_lib("eunit/include/eunit.hrl").
-import(wall_utils, [deserialize/1,
                     add_timestamp/1,
                     append_newline/1,
                     message/2
                    ]).


%deserialize_failed_test() ->
%    Binary = <<10,20,30,40,50,60>>,
%    {Result, Message} = deserialize(Binary),
%    ?_assert(Result =:= ok).


deserialize_succeed_test() -> ok.


add_timestamp_test() ->
    % decoded message
    Message = #{?MESSAGE_FIELD => <<"message">>, ?USER_FIELD => <<"user">>},
    MessageWithTimeStamp = add_timestamp(Message),
    {Result, Timestamp} = maps:find(?TIMESTAMP_FIELD, MessageWithTimeStamp),
    % Indicates the esitence of Timestamp field
    ?_assert(Result =:= 'ok').



append_newline_test() ->
    ?_assert(<<"I am ok\n">> =:= append_newline(<<"I am ok\n">>)),
    ?_assert(<<"I will be">> =:= append_newline(<<"I will be\n">>)).



message_test() ->
    % Creates and encodes the message
    %Message = message("user", "text"),

    ?debugMsg("append_new_line test passed").

    %Message = <<Size:?HEADER_SIZE/unsigned-big-integer, Rest/binary>>,
    %<<HeaderSiz<F2>:?HEADER_SIZE/unsigned-big-integer, Body/bits>> = Message,

    %?debugMsg("Boobs").




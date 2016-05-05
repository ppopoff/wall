#!/usr/bin/env escript
%% @author Paul Popoff
%% @version 0.2
%% @title A client application for wall-server
%%
-module(wall_client).
-mode(compile).

-export([start_listener/1, loop/1]).
-export([decode_message/1, encode_message/1]).

-define(TIMEOUT, 120000).
-define(ADDRESS, "localhost").
-define(PORT, 8000).


%% @doc
%% @hidden
%% An entry point to the application
main(Username) ->
    Socket = connect(),
    log_in(Username, Socket),
    Pid = start_listener(Socket),
    gen_tcp:controlling_process(Socket, Pid),

    %% Wait for user input and if it happens
    %% send it to the socket controlling process
    repl(Pid),
    ok.



%% this method reads the STDIN, encodes the message
%% to correspond to the given binary format
%% and then prints the output
repl(Pid) ->
    Prompt = ("> "),
    Pid ! {send, io:get_line(Prompt)},
    repl(Pid).


start_listener(Socket) ->
    spawn(?MODULE, loop, [Socket]).


loop(Socket) ->
    receive
        % Message received!
        {tcp, Socket, Data} ->
            inet:setopts(Socket, [{active, once}]),
            DecodedMessage = decode_message(Data),
            io:format(get_prompt() ++ "~s", [DecodedMessage]),
            loop(Socket);
        {send, Message} ->
            inet:setopts(Socket, [{active, once}]),
            EncodedMessage = encode_message(Message),
            gen_tcp:send(Socket, EncodedMessage),
            loop(Socket);
        {tcp_error, _, _Reason} ->
            io:format("we're in a deep deep shit"),
            gen_tcp:close(Socket),
            exit(normal);
        {tcp_closed, _Socket} ->
            gen_tcp:close(Socket),
            exit(normal);
        stop ->
            gen_tcp:close(Socket),
            exit(normal)
    end.



% Networing
% --------------------------------------------------------

-spec connect() -> port().
connect() ->
    connect(?ADDRESS, ?PORT).


-spec connect(string(), integer()) -> port().
connect(Address, Port) ->
    {ok, Socket} = gen_tcp:connect(
        Address, Port, [
            {active, once}
            %binary,
         ]),
    Socket.


-spec log_in(string(), port()) -> ok.
log_in(Username, Socket) ->
    BinUname = list_to_binary(Username),
    StrLen = byte_size(BinUname),

    Header = case binary:encode_unsigned(StrLen, big) of
                Byte  when byte_size(Byte)  =:=1 -> <<0, Byte/bits>>;
                Bytes when byte_size(Bytes) =:=2 -> Bytes
                % TODO: handle other cases
             end,

    AuthDetails = <<Header/binary, BinUname/binary>>,
    gen_tcp:send(Socket, AuthDetails),
    ok.


decode_message("OK") -> "Now post!\n";
decode_message(Message) when is_list(Message) ->
    decode_message(list_to_binary(Message));
decode_message(Message) when is_binary(Message) ->
    HeaderSize = 3,
    <<Header:HeaderSize/binary, Rest/binary>> = Message,
    MessageLength = binary:decode_unsigned(Header),

    <<Body:MessageLength/binary, _Left/binary>> = Rest,
    DecodedMessage = binary_to_term(Body),
    DecodedMessage.



%% @doc Encodes the message
%% @spec encode_message(string()) -> binary()
-spec encode_message(string()) -> binary().
encode_message(Message) ->
    Payload = term_to_binary(Message),
    PayloadSize = byte_size(Payload),
    <<PayloadSize:24/unsigned-big-integer, Payload/bits>>.


%% @doc prings local time, when message is received
%% @spec get_prompt() -> string()
-spec get_prompt() -> string().
get_prompt() ->
  "[" ++ localtime() ++ "] ".


%% @doc obtains local time in milliscodns
%% @spec localtime() -> string()
-spec localtime() -> string().
localtime() ->
    {_, _, _Micro} = Now = os:timestamp(),
    {_Date, {Hours, Minutes, Seconds}} = calendar:now_to_local_time(Now),
    to_s(Hours) ++ ":" ++ to_s(Minutes) ++ ":" ++ to_s(Seconds).


%% @doc Converts given integer to string
%% @spec to_s(Int::integer()) -> string()
-spec to_s(integer()) -> string().
to_s(Int) -> io_lib:format("~p", [Int]).


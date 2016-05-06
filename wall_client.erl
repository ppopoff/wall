#!/usr/bin/env escript
%% @author Paul Popoff
%% @version 0.2
%% @title A client application for wall-server
%%
-module(wall_client).
-mode(compile).

-export([start_listener/1, loop/1]).
-export([decode_message/1, encode_message/2]).

-define(TIMEOUT, 120000).
-define(ADDRESS, "localhost").
-define(PORT, 8000).


-record(state, {
    socket  :: port(),
    username :: binary()
}).

-type state() :: #state{}.


%% @doc
%% @hidden
%% An entry point to the application
main(Username) ->
    Socket = connect(),
    State = #state{socket = Socket, username = Username},

    log_in(State),
    Pid = start_listener(State),
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


start_listener(State) ->
    spawn(?MODULE, loop, [State]).


-spec loop(state()) -> none().
loop(State=#state{username = Username, socket = Socket}) ->
    receive
        % Message received!
        {tcp, Socket, Data} ->
            inet:setopts(Socket, [{active, once}]),

            case Data of
                "OK" -> io:format("Now post! ~n");
                BinaryData ->
                    io:format("Accepted: ~tp ~n", [BinaryData]),
                    DecodedMessage = decode_message(BinaryData),
                    {ok, MessageText} = maps:find("m", DecodedMessage),
                    {ok, User} = maps:find("u", DecodedMessage),
                    io:format(get_prompt() ++ "~s " ++ "~s", [User, MessageText])
            end,

            loop(State);
        {send, Message} ->
            inet:setopts(Socket, [{active, once}]),
            EncodedMessage = encode_message(Username, Message),
            gen_tcp:send(Socket, EncodedMessage),
            loop(State);
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


%-spec log_in(tate=#state{username=b}) -> ok.
log_in(_State=#state{username=Username, socket=Socket}) ->
    BinUname = list_to_binary(Username),
    StrLen = byte_size(BinUname),

    %TODO use different approach to the auth
    Header = case binary:encode_unsigned(StrLen, big) of
                Byte  when byte_size(Byte)  =:=1 -> <<0, Byte/bits>>;
                Bytes when byte_size(Bytes) =:=2 -> Bytes
                % TODO: handle other cases
             end,

    AuthDetails = <<Header/binary, BinUname/binary>>,
    gen_tcp:send(Socket, AuthDetails),
    ok.



%% TODO eliminate this: Accept only binary not a list
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
%% @spec encode_message(string(), string()) -> binary()
-spec encode_message(string(), string()) -> binary().
encode_message(Username, Message) ->
    Payload = #{"m" => Message, "u" => Username},
    EncodedPayload = term_to_binary(Payload),
    PayloadSize = byte_size(EncodedPayload),
    <<PayloadSize:24/unsigned-big-integer, EncodedPayload/bits>>.


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


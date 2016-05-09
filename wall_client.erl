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
loop(State = #state{username = Username, socket = Socket}) ->
    receive
        % Message received!
        {tcp, Socket, Data} ->
            inet:setopts(Socket, [{active, once}]),

            DecodedMessage = decode_message(Data),
            {ok, MessageText} = maps:find(<<"m">>, DecodedMessage),
            {ok, DateTime}    = maps:find(<<"t">>, DecodedMessage),
            {ok, SenderName}  = maps:find(<<"u">>, DecodedMessage),
            io:format(pretty_print(DateTime, SenderName, MessageText)),

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


%% @doc Sends authentication request to the server
%% @spec log_in(state()) -> ok.
-spec log_in(state()) -> ok.
log_in(_State=#state{username = Username, socket = Socket}) ->
    gen_tcp:send(Socket, encode_message(Username, "auth")),
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
    EncodedPayload  = term_to_binary(#{<<"m">> => list_to_binary(Message), <<"u">> => list_to_binary(Username)}),
    PayloadSize     = byte_size(EncodedPayload),
    <<PayloadSize:24/unsigned-big-integer, EncodedPayload/bits>>.


-spec pretty_print(any(), string(), string()) -> string().
pretty_print(DateTime, SenderName, Text) ->
    get_prompt(DateTime, binary_to_list(SenderName)) ++ binary_to_list(Text).


%% @doc prings local time, when message is received
%% @spec get_prompt(any(), string()) -> string().
-spec get_prompt(any(), string()) -> string().
get_prompt(DateTime, SenderName) ->
  "[" ++ format_datetime(DateTime) ++ "] " ++ SenderName ++ ": ".

%TODO specify datetime format as a type

%% @doc Formats given datetime object
%% @spec format_datetime(any()) -> string()
-spec format_datetime(any()) -> string().
format_datetime(LocalTime) ->
    {_Date, {Hours, Minutes, Seconds}} = LocalTime,
    to_s(Hours) ++ ":" ++ to_s(Minutes) ++ ":" ++ to_s(Seconds).


%TODO: support 2digit formats
%% @doc Converts given integer to string
%% @spec to_s(Int::integer()) -> string()
-spec to_s(integer()) -> string().
to_s(Int) -> io_lib:format("~p", [Int]).


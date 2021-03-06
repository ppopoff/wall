#!/usr/bin/env escript
%% @author Paul Popoff
%% @version 0.2
%% @title A client application for wall-server
%%
-module(wall_client).
-author(ppopoff).

-mode(compile).

-export([start_listener/1, loop/1]).

-define(ADDRESS, "localhost").
-define(PORT, 8000).

%% Text colors
-define(PRMPT_TEXT, "\e[;93m").
-define(BUDDY_TEXT, "\e[;91m").
-define(RESET_TEXT,"\e[0m").

% Message map fields
-define(TIMESTAMP_FIELD, <<"t">>).
-define(MESSAGE_FIELD,   <<"m">>).
-define(USER_FIELD,      <<"u">>).

-define(AUTH_REQ,    <<"auth">>).
-define(AUTH_REQ_S,  "auth").
-define(AUTH_RES,    <<"ok">>).
-define(FROM_SERVER, <<"server">>).

%% Heades size must be 3 bytes long
-define(HEADER_SIZE, 24).
-define(BYTE, 8).

%% Default timeout value (for not it's hardcoded)
-define(TIMEOUT, infinity).

-record(state, {
    socket         :: port(),
    username       :: username(),
    buffer = <<>>  :: binary()
}).

-type state() :: #state{}.
-type addr()  :: inet:ip_address() | inet:hostname().
-type sent()  :: ok | {error, closed | inet:posix()}.
-type localtime() :: any().
-type message()   :: map().
-type username()  :: binary().
-type transport() :: any().




%% @doc
%% An entry point to the application
main(Username) ->
    Socket = connect(?ADDRESS, ?PORT),
    State = #state{socket = Socket, username = Username},

    log_in(State),
    Pid = start_listener(State),

    % process that will receive handle_info messages
    gen_tcp:controlling_process(Socket, Pid),

    repl(Pid),
    ok.


%% @private @doc Reads message from STDIN, and sends it
%% to internal process that performs network ops
repl(Pid) ->
    Pid ! {send, io:get_line(fmt_user_input())},
    repl(Pid).


%% @doc Starts Process that is responsible
%% network operations
start_listener(State) ->
    spawn(?MODULE, loop, [State]).


%% @private @doc Process loop.
-spec loop(state()) -> none().
loop(State = #state{username = Username, socket = Socket, buffer = Buffer}) ->
    receive
        %% Message received
        {tcp, Socket, Data} ->
            inet:setopts(Socket, [{active, once}]),
            BytesReceived = <<Buffer/binary, Data/binary>>,
            loop(decode_data(BytesReceived, State));
        %% Message sent
        {send, Message} ->
            send_message(Socket, Username, Message),
            loop(State);
        %% Tcp error occured
        {tcp_error, _, _Reason} ->
            io:format("tcp error happened"),
            exit(normal);
        %% Tcp connection closed
        {tcp_closed, _Socket} ->
            exit(normal);
        %% stop signal was sent
        stop ->
            gen_tcp:close(Socket),
            exit(normal)
    end.


%% @private @doc Decodes and handles the message
-spec decode_data(Data :: binary(), State :: state()) -> state().
decode_data(Data = <<Size:?HEADER_SIZE/unsigned-big-integer, Rest/binary>>, State) ->
    case byte_size(Rest) >= Size of
        true -> % We have at least one message
            <<MessageBody:Size/binary, Left/binary>> = Rest,

            %% Handle the message
            handle_message(MessageBody),

            %% If anything left it will be decoded next
            decode_data(Left, State);
        false -> % Waiting for more stuff to come
            State#state{buffer = Data}
    end;
decode_data(Data, State) ->
    State#state{buffer = Data}.


%% @private @doc Handles the received and decoded message
-spec handle_message(BinaryMessage :: binary()) -> ok.
handle_message(BinaryMessage) ->
    DecodedMessage = binary_to_term(BinaryMessage),
    {ok, MessageText} = maps:find(?MESSAGE_FIELD, DecodedMessage),
    {ok, DateTime}    = maps:find(?TIMESTAMP_FIELD, DecodedMessage),
    {ok, SenderName}  = maps:find(?USER_FIELD, DecodedMessage),
    io:format(pretty_print(DateTime, SenderName, MessageText)).


%% @private @doc Creates the connections
-spec connect(Host :: addr(), Port :: integer()) -> port().
connect(Host, Port) ->
    {ok, Socket} = gen_tcp:connect(Host, Port, [{active, once}, binary]),
    Socket.


%% @private @doc Sends authentication request to the server
-spec log_in(state()) -> sent().
log_in(_State = #state{username = Username, socket = Socket}) ->
    gen_tcp:send(Socket, encode_message(Username, ?AUTH_REQ_S)).


%% @private @doc Sends message to the given socket
-spec send_message(Socket :: port(), Username :: string(), Message :: string()) -> sent().
send_message(Socket, Username, Message) ->
    inet:setopts(Socket, [{active, once}]),
    EncodedMessage = encode_message(Username, Message),
    gen_tcp:send(Socket, EncodedMessage).


%% @private @doc Creates the message and encodes it.
-spec encode_message(Username :: string(), Message :: string()) -> binary().
encode_message(Username, Message) ->
    EncodedPayload  = term_to_binary(#{
        ?MESSAGE_FIELD => list_to_binary(Message),
        ?USER_FIELD    => list_to_binary(Username)
    }),
    PayloadSize = byte_size(EncodedPayload),
    <<PayloadSize:?HEADER_SIZE/unsigned-big-integer, EncodedPayload/bits>>.


%% @private @doc Formats message for the terminal
-spec pretty_print(DateTime :: localtime(), Sender :: binary(), MessageText :: binary()) -> string().
pretty_print(DateTime, Sender, MessageText) ->
    Text   = binary_to_list(MessageText),
    ?BUDDY_TEXT ++ fmt_prompt(DateTime, Sender) ++ Text ++ ?RESET_TEXT.


%% @private @doc prings local time, when message is received
-spec fmt_prompt(DateTime :: localtime(), string()) -> string().
fmt_prompt(DateTime, SenderName) ->
    Sender = binary_to_list(SenderName),
    "[" ++ fmt_datetime(DateTime) ++ "] " ++ Sender ++ ": ".


%% @private @doc Formats given datetime object
-spec fmt_datetime(LocalTime :: localtime()) -> string().
fmt_datetime(LocalTime) ->
    {_Date, {Hours, Minutes, Seconds}} = LocalTime,
    to_s(Hours) ++ ":" ++ to_s(Minutes) ++ ":" ++ to_s(Seconds).


%% @private @doc Formats user prompt
-spec fmt_user_input() -> string().
fmt_user_input() ->
    ?RESET_TEXT ++ "> " ++ ?PRMPT_TEXT.


%% @private @doc Converts given integer to string with padding == 2:w
-spec to_s(Int :: integer()) -> string().
to_s(Int) -> io_lib:format("~2..0B", [Int]).


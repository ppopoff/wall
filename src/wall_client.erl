-module(wall_client).

-export([run/1]).
-export([disconnect/1]).
-export([start_listener/2, loop/2]).
-export([decode_message/1, encode_message/1]).

-define(TIMEOUT, 120000).
-define(ADDRESS, "localhost").
-define(PORT, 8000).


%% An entry point to the application
run(Username) ->
    Socket = connect(),
    log_in(Username, Socket),
    Pid = start_listener(Socket, Username),
    gen_tcp:controlling_process(Socket, Pid),

    %% Wait for user input and if it happens
    %% send it to the socket controlling process
    repl(Pid).



%% this method reads the STDIN, encodes the message
%% to correspond to the given binary format
%% and then prints the output
repl(Pid) ->
  Prompt = ("> "),
  Pid ! {send, encode_message(io:get_line(Prompt))},
  repl(Pid).


start_listener(Socket, Username) ->
    spawn(main, loop, [Socket, Username]).

loop(Socket, Username) ->
    receive
        % Message received!
        {tcp, Socket, Data} ->
            %DecodedMessage = decode_message(Data),
            io:format(">> ~tp ~n", [Data]),
            %io:format("> ~tp~n", [DecodedMessage]),
            inet:setopts(Socket, [{active, once}]),
            loop(Socket, Username);
        {send, Message} ->
            inet:setopts(Socket, [{active, once}]),
            EncodedMessage = encode_message(Message),
            gen_tcp:send(Socket, EncodedMessage),
            loop(Socket, Username);
        {tcp_error, _, _Reason} ->
            io:format("we're in a deep deep shit"),
            exit(normal);
        {tcp_closed, _Socket} ->
            exit(normal);
        stop ->
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


-spec disconnect(port()) -> ok.
disconnect(Socket) -> gen_tcp:close(Socket).


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


% Message encoding/decoding
% --------------------------------------------------------

-spec encode_message(string()) -> binary().
encode_message(Message) ->
    Payload = term_to_binary(Message),
    PayloadSize = byte_size(Payload),

    Header = case binary:encode_unsigned(PayloadSize, big) of
                Byte  when byte_size(Byte)  =:=1 -> <<0, 0, Byte/bits>>;
                Bytes when byte_size(Bytes) =:=2 -> <<0, Bytes/bits>>;
                Bytes when byte_size(Bytes) =:=3 -> Bytes
             end,

    <<Header/binary, Payload/binary>>.


-spec decode_message(binary()) -> string().
decode_message(Message) ->
    HeaderSize = 3,
    <<Header:HeaderSize/binary, Rest/binary>> = Message,
    io:format("Header ~tp~n", [Header]),
    MessageLength = binary:decode_unsigned(Header),
    io:format("Message size is: ~tp~n", [MessageLength]),

    <<Body:MessageLength/binary, _Left/binary>> = Rest,
    DecodedMessage = binary_to_term(Body),
    io:format("Decoded message ~tp~n", [DecodedMessage]),
    DecodedMessage.



% Prompt utils
% --------------------------------------------------------
-spec get_prompt(integer()) -> integer().
get_prompt(Username) ->
  "[" ++ localtime_ms() ++"] " ++ Username ++ ": ".


% timestamp utils
% --------------------------------------------------------
-spec localtime_ms() -> string().
localtime_ms() ->
    {_, _, _Micro} = Now = os:timestamp(),
    {_Date, {Hours, Minutes, Seconds}} = calendar:now_to_local_time(Now),
    to_s(Hours) ++ ":" ++ to_s(Minutes) ++ ":" ++ to_s(Seconds).


% utilities
% ----------------------------------------------------------
-spec to_s(integer()) -> string().
to_s(Int) -> io_lib:format("~p", [Int]).


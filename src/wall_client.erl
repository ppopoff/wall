-module(wall_client).

-export([run/1]).
-export([disconnect/1]).
-export([start_listener/2, loop/2]).

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
            inet:setopts(Socket, [{active, once}]),

            case Data of

                % Authentication suceed
                <<"OK\n">> ->
                    io:format("Welcome, ~tp~n", [Username]),
                    loop(Socket, Username);

                % Message that should be prited
                Message ->
                    DecodedMessage = decode_message(Message),
                    io:format("> ~tp~n", [DecodedMessage]),
                    loop(Socket, Username)
            end;
        {send, Message} ->
            inet:setopts(Socket, [{active, once}]),
            WellFormedMessage = <<Message/binary>>,
            gen_tcp:send(Socket, WellFormedMessage),
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
    AuthDetails = <<BinUname/binary, "\r\n">>,
    gen_tcp:send(Socket, AuthDetails),
    ok.


% Message encoding/decoding
% --------------------------------------------------------

-spec encode_message(string()) -> binary().
encode_message(Message) -> list_to_binary(Message).


-spec decode_message(binary()) -> binary().
decode_message(Message) -> Message.



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


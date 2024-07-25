-module(go_modem_connection).
-behaviour(gen_statem).

-include("go_modem.hrl").

%% API
-export([start_link/3]).

%% gen_statem callbacks
-export([
    init/1,
    callback_mode/0
]).

%% state functions
-export([
    neutral/3
]).

-define(MESSAGE_BYTE_SIZE, 4).

%%%
%%% API
%%%

start_link(IoDevice, Server, ServerMod) ->
    gen_statem:start_link(?MODULE, [IoDevice, Server, ServerMod], []).

%%%
%%% gen_statem callbacks
%%%

init([IoDevice, Server, ServerMod]) ->
    Self = self(),
    _Reader = spawn_link(fun() -> read_loop(IoDevice, Self) end),
    Data = #{
        io_device => IoDevice,
        server => Server,
        server_module => ServerMod
    },
    {ok, neutral, Data}.

callback_mode() -> state_functions.

%%%
%%% Neutral state
%%%

neutral(cast, {message, MessageBytes}, Data) ->
    #{io_device := IoDevice} = Data,

    Message = go_modem_protocol:decode_message(MessageBytes),
    #message{sender_seq_id = S, receiver_seq_id = R, command = Command} = Message,
    logger:info("Sender sequence bit: ~p, receiver sequence bit: ~p", [S, R]),

    case handle_command(Command, Data) of
        {noreply, NewData} ->
            {keep_state, NewData};
        {reply, Response, NewData} ->
            ResponseMessage = #message{sender_seq_id = R, receiver_seq_id = S, command = Response},
            ResponseBytes = go_modem_protocol:encode_message(ResponseMessage),
            ok = file:write(IoDevice, ResponseBytes),
            {keep_state, NewData}
    end.

%%%
%%% OK Wait state
%%%

%%%
%%% Private functions
%%%

read_loop(IoDevice, Connection) ->
    {ok, Bytes} = file:read(IoDevice, ?MESSAGE_BYTE_SIZE),
    ok = gen_statem:cast(Connection, {message, Bytes}),
    read_loop(IoDevice, Connection).

handle_command(new_game, Data) ->
    #{server := Server, server_module := ServerMod} = Data,
    Reply = ServerMod:new_game(Server),
    {reply, Reply, Data};
handle_command({move, Player, Move}, Data) ->
    #{server := Server, server_module := ServerMod} = Data,
    Reply = ServerMod:move(Server, Player, Move),
    {reply, Reply, Data}.

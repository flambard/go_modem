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
        server_module => ServerMod,
        my_seq_id => 0,
        opponent_seq_id => 0
    },
    {ok, neutral, Data}.

callback_mode() -> state_functions.

%%%
%%% Neutral state
%%%

neutral(cast, {recv_message, MessageBytes}, Data) ->
    #{
        io_device := IoDevice,
        my_seq_id := MySeqID,
        opponent_seq_id := OpponentSeqID
    } = Data,

    Message = go_modem_protocol:decode_message(MessageBytes),

    %% TODO: If my seq ID does not match with receiver seq ID, see spec what to do
    #message{
        send_seq_id = SenderSeqID,
        recv_seq_id = MySeqID,
        command = Command
    } = Message,

    %% TODO: If repeated ID, discard the message
    SenderSeqID = (OpponentSeqID + 1) rem 2,

    case handle_command(Command, Data) of
        {noreply, NewData} ->
            {keep_state, NewData#{opponent_seq_id => SenderSeqID}};
        {reply, Response, NewData} ->
            ResponseMessage = #message{
                send_seq_id = MySeqID,
                recv_seq_id = SenderSeqID,
                command = Response
            },
            ResponseBytes = go_modem_protocol:encode_message(ResponseMessage),
            ok = send(IoDevice, ResponseBytes),
            {keep_state, NewData#{opponent_seq_id => SenderSeqID}}
    end.

%%%
%%% OK Wait state
%%%

%%%
%%% Private functions
%%%

read_loop(IoDevice, Connection) ->
    {ok, Bytes} = file:read(IoDevice, ?MESSAGE_BYTE_SIZE),
    ok = gen_statem:cast(Connection, {recv_message, Bytes}),
    read_loop(IoDevice, Connection).

send(IoDevice, Bytes) ->
    ok = file:write(IoDevice, Bytes).

handle_command(new_game, Data) ->
    #{server := Server, server_module := ServerMod} = Data,
    Reply = ServerMod:new_game(Server),
    {reply, Reply, Data};
handle_command({move, Player, Move}, Data) ->
    #{server := Server, server_module := ServerMod} = Data,
    Reply = ServerMod:move(Server, Player, Move),
    {reply, Reply, Data}.

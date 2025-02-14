-module(go_modem_connection).
-behaviour(gen_statem).

-include("go_modem.hrl").

%% API
-export([
    start_link/3,
    send_command/2
]).

%% gen_statem callbacks
-export([
    init/1,
    callback_mode/0
]).

%% state functions
-export([
    neutral/3,
    ok_wait/3
]).

-define(MESSAGE_BYTE_SIZE, 4).

%%%
%%% API
%%%

start_link(IoDevice, Server, ServerMod) ->
    gen_statem:start_link(?MODULE, [IoDevice, Server, ServerMod], []).

send_command(Server, Command) ->
    gen_statem:call(Server, {send_command, Command}).

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
        opponent_seq_id => 0,
        outstanding_query => undefined,
        reply_to => undefined
    },
    {ok, neutral, Data}.

callback_mode() -> state_functions.

%%%
%%% Neutral state
%%%

neutral({call, From}, {send_command, Command}, Data) ->
    #{
        io_device := IoDevice,
        my_seq_id := MySeqID,
        opponent_seq_id := OpponentSeqID
    } = Data,

    MyNewSeqID = (MySeqID + 1) rem 2,

    Message = #message{
        send_seq_id = MyNewSeqID,
        recv_seq_id = OpponentSeqID,
        command = Command
    },

    MessageBytes = go_modem_protocol:encode_message(Message),
    ok = send_message(IoDevice, MessageBytes),

    NewData =
        case Command of
            {query, Query} -> Data#{reply_to => From, outstanding_query => Query};
            _ -> Data#{reply_to => From}
        end,
    {next_state, ok_wait, NewData#{my_seq_id => MyNewSeqID}};
neutral(cast, {recv_message, MessageBytes}, Data) ->
    #{
        io_device := IoDevice,
        my_seq_id := MySeqID,
        opponent_seq_id := OpponentSeqID
    } = Data,

    logger:info("Message while in Neutral. My ID: ~p, Opponent ID: ~p", [MySeqID, OpponentSeqID]),

    NextSenderSeqID = (OpponentSeqID + 1) rem 2,

    case go_modem_protocol:decode_message(MessageBytes) of
        #message{command = ok} ->
            %% OK in neutral state - discard
            keep_state_and_data;
        #message{command = deny} ->
            %% DENY in neutral state - discard
            keep_state_and_data;
        #message{recv_seq_id = RecvSeqID} when RecvSeqID =/= MySeqID ->
            %% New or old command not possible to receive - discard
            keep_state_and_data;
        #message{send_seq_id = OpponentSeqID} ->
            %% Old command
            %% TODO: resend last command (implicit or real OK)
            keep_state_and_data;
        #message{
            send_seq_id = NextSenderSeqID,
            recv_seq_id = MySeqID,
            command = Command
        } ->
            case handle_command(Command, Data) of
                {noreply, NewData} ->
                    {keep_state, NewData#{opponent_seq_id => NextSenderSeqID}};
                {reply, Response, NewData} ->
                    ResponseMessage = #message{
                        send_seq_id = MySeqID,
                        recv_seq_id = NextSenderSeqID,
                        command = Response
                    },
                    ResponseBytes = go_modem_protocol:encode_message(ResponseMessage),
                    ok = send_message(IoDevice, ResponseBytes),
                    {keep_state, NewData#{opponent_seq_id => NextSenderSeqID}}
            end
    end.

%%%
%%% OK Wait state
%%%

ok_wait(cast, {recv_message, MessageBytes}, Data) ->
    #{
        io_device := IoDevice,
        my_seq_id := MySeqID,
        opponent_seq_id := OpponentSeqID,
        reply_to := ReplyTo
    } = Data,

    logger:info("Message while waiting for OK. My ID: ~p, Opponent ID: ~p", [MySeqID, OpponentSeqID]),

    NextSenderSeqID = (OpponentSeqID + 1) rem 2,

    case go_modem_protocol:decode_message(MessageBytes) of
        #message{
            send_seq_id = OpponentSeqID,
            recv_seq_id = MySeqID,
            command = ok
        } ->
            gen_statem:reply(ReplyTo, ok),
            {next_state, neutral, Data};
        #message{command = ok} ->
            %% Not possible - discard
            keep_state_and_data;
        #message{
            send_seq_id = OpponentSeqID,
            recv_seq_id = MySeqID,
            command = deny
        } ->
            gen_statem:reply(ReplyTo, deny),
            {next_state, neutral, Data};
        #message{command = deny} ->
            %% Not possible - discard
            keep_state_and_data;
        #message{
            send_seq_id = NextSenderSeqID,
            recv_seq_id = MySeqID,
            command = {answer, Answer}
        } ->
            {reply, Response, NewData} = handle_command({answer, Answer}, Data),
            ResponseMessage = #message{
                send_seq_id = MySeqID,
                recv_seq_id = NextSenderSeqID,
                command = Response
            },
            ResponseBytes = go_modem_protocol:encode_message(ResponseMessage),
            ok = send_message(IoDevice, ResponseBytes),
            {next_state, neutral, NewData#{opponent_seq_id => NextSenderSeqID}};
        %% TODO: If seq IDs does not match with expected IDs, see spec what to do
        #message{
            send_seq_id = NextSenderSeqID,
            recv_seq_id = MySeqID,
            command = Command
        } ->
            %% TODO: Maybe not handle commands generally in state ok_wait
            case handle_command(Command, Data) of
                {noreply, NewData} ->
                    {next_state, neutral, NewData};
                {reply, Response, NewData} ->
                    ResponseMessage = #message{
                        send_seq_id = MySeqID,
                        recv_seq_id = NextSenderSeqID,
                        command = Response
                    },
                    ResponseBytes = go_modem_protocol:encode_message(ResponseMessage),
                    ok = send_message(IoDevice, ResponseBytes),
                    {next_state, neutral, NewData#{opponent_seq_id => NextSenderSeqID}}
            end
    end.

%%%
%%% Private functions
%%%

read_loop(IoDevice, Connection) ->
    {ok, Bytes} = file:read(IoDevice, ?MESSAGE_BYTE_SIZE),
    ok = gen_statem:cast(Connection, {recv_message, Bytes}),
    read_loop(IoDevice, Connection).

send_message(IoDevice, Bytes) ->
    ok = file:write(IoDevice, Bytes).

handle_command(new_game, Data) ->
    #{server := Server, server_module := ServerMod} = Data,
    Reply = ServerMod:handle_new_game(Server),
    {reply, Reply, Data};
handle_command({move, Player, Move}, Data) ->
    #{server := Server, server_module := ServerMod} = Data,
    Reply = ServerMod:handle_move(Server, Player, Move),
    {reply, Reply, Data};
handle_command({query, Query}, Data) ->
    #{server := Server, server_module := ServerMod} = Data,
    Reply = ServerMod:handle_query(Server, Query),
    {reply, Reply, Data};
handle_command({answer, AnswerBits}, Data) ->
    #{
        server := _Server,
        server_module := _ServerMod,
        outstanding_query := Query,
        reply_to := ReplyTo
    } = Data,
    Answer = go_modem_query:decode_answer(Query, AnswerBits),
    logger:info("Replying with ANSWER ~p to ~p", [Answer, ReplyTo]),
    ok = gen_statem:reply(ReplyTo, Answer),
    %% Reply = ok/deny
    Reply = ok,
    NewData = Data#{outstanding_query => undefined, reply_to => undefined},
    {reply, Reply, NewData}.

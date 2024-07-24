-module(go_modem_connection).
-behaviour(gen_server).

-include("go_modem.hrl").

%% API
-export([start_link/3]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2
]).

-define(MESSAGE_BYTE_SIZE, 4).

%%%
%%% API
%%%

start_link(IoDevice, Server, ServerMod) ->
    gen_server:start_link(?MODULE, [IoDevice, Server, ServerMod], []).

%%%
%%% gen_server callbacks
%%%

init([IoDevice, Server, ServerMod]) ->
    Self = self(),
    _Reader = spawn_link(fun() -> read_loop(IoDevice, Self) end),
    State = #{
        io_device => IoDevice,
        server => Server,
        server_module => ServerMod
    },
    {ok, State}.

handle_call(_Call, _From, State) ->
    {reply, ok, State}.

handle_cast({message, MessageBytes}, State) ->
    #{io_device := IoDevice} = State,

    Message = go_modem_protocol:decode_message(MessageBytes),
    #message{his = H, yours = Y, command = Command} = Message,
    logger:info("His sequence bit: ~p, your sequence bit: ~p", [H, Y]),

    case handle_command(Command, State) of
        {noreply, NewState} ->
            {noreply, NewState};
        {reply, Response, NewState} ->
            ResponseMessage = #message{his = Y, yours = H, command = Response},
            ResponseBytes = go_modem_protocol:encode_message(ResponseMessage),
            ok = file:write(IoDevice, ResponseBytes),
            {noreply, NewState}
    end.

%%%
%%% Private functions
%%%

read_loop(IoDevice, Connection) ->
    {ok, Bytes} = file:read(IoDevice, ?MESSAGE_BYTE_SIZE),
    ok = gen_server:cast(Connection, {message, Bytes}),
    read_loop(IoDevice, Connection).

handle_command(new_game, State) ->
    #{server := Server, server_module := ServerMod} = State,
    Reply = ServerMod:new_game(Server),
    {reply, Reply, State};
handle_command({move, Player, Move}, State) ->
    #{server := Server, server_module := ServerMod} = State,
    Reply = ServerMod:move(Server, Player, Move),
    {reply, Reply, State}.

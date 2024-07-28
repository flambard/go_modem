-module(go_modem_server_mock).
-behaviour(gen_server).
-behaviour(go_modem_server).

%% API
-export([
    start_link/1,
    call/2,
    connection_pid/1
]).

%% go_modem_server callbacks
-export([
    handle_new_game/1,
    handle_move/3,
    handle_query/2
]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2
]).

%%%
%%% API
%%%

start_link(IoDevice) ->
    gen_server:start_link(?MODULE, [IoDevice, self()], []).

call(Server, Fun) ->
    gen_server:call(Server, {make_call, Fun}).

connection_pid(Server) ->
    gen_server:call(Server, connection_pid).

%%%
%%% go_modem_server
%%%

handle_new_game(Server) ->
    gen_server:call(Server, {callback, handle_new_game}).

handle_move(Server, Player, Move) ->
    gen_server:call(Server, {callback, {handle_move, Player, Move}}).

handle_query(Server, Query) ->
    gen_server:call(Server, {callback, {query, Query}}).

%%%
%%% gen_server callbacks
%%%

init([IoDevice, Owner]) ->
    {ok, Pid} = go_modem_connection:start_link(IoDevice, self(), ?MODULE),
    %% {ok, Pid} = go_modem:start_connection(IoDevice, self(), ?MODULE),
    State = #{
        connection => Pid,
        owner => Owner
    },
    {ok, State}.

handle_call({make_call, Fun}, _From, State) ->
    #{connection := Pid} = State,
    {reply, Fun(Pid), State};
handle_call(connection_pid, _From, State) ->
    #{connection := Pid} = State,
    {reply, Pid, State};
handle_call({callback, Callback}, _From, State) ->
    #{owner := Owner} = State,
    Ref = make_ref(),
    Owner ! {callback, self(), Ref, Callback},
    receive
        {reply, Ref, Reply} -> {reply, Reply, State}
    end.

handle_cast(_Message, State) ->
    {noreply, State}.

-module(go_modem_roundtrip_tests).

-include_lib("eunit/include/eunit.hrl").

newgame_test() ->
    {ok, CrossoverIO} = crossover_io_server:start_link([]),

    {ok, Receiver} = go_modem_server_mock:start_link(CrossoverIO),
    {ok, Sender} = go_modem_server_mock:start_link(CrossoverIO),

    ReceiverConnection = go_modem_server_mock:connection_pid(Receiver),
    SenderConnection = go_modem_server_mock:connection_pid(Sender),

    ok = crossover_io_server:add_writer(CrossoverIO, client_a, ReceiverConnection),
    ok = crossover_io_server:add_writer(CrossoverIO, client_b, SenderConnection),

    TestProcess = self(),

    spawn_link(fun() ->
        Reply = go_modem_server_mock:call(Sender, fun(Connection) ->
            go_modem_connection:send_command(Connection, new_game)
        end),
        TestProcess ! {new_game_reply, Reply}
    end),

    receive
        {callback, Receiver, Ref, handle_new_game} -> Receiver ! {reply, Ref, ok}
    end,

    receive
        {new_game_reply, ok} -> ok
    end.

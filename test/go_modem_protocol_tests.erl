-module(go_modem_protocol_tests).

-include_lib("eunit/include/eunit.hrl").
-include("go_modem.hrl").

encode_new_game_message_test() ->
    Message = #message{send_seq_id = 1, recv_seq_id = 0, command = new_game},
    IoData = go_modem_protocol:encode_message(Message),
    <<1, 161, 160, 128>> = iolist_to_binary(IoData).

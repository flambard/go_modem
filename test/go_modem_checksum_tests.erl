-module(go_modem_checksum_tests).

-include_lib("eunit/include/eunit.hrl").

calculate_test() ->
    <<161>> = go_modem_checksum:calculate(<<1>>, <<160, 128>>).

calculate_modulo_test() ->
    <<134>> = go_modem_checksum:calculate(<<0>>, <<210, 180>>).

valid_checksum_test() ->
    true = go_modem_checksum:is_valid(<<1, 161, 160, 128>>).

checksum_modulo_test() ->
    true = go_modem_checksum:is_valid(<<0, 134, 210, 180>>).

-module(go_modem_checksum).

-include_lib("eunit/include/eunit.hrl").

-export([
    calculate/2,
    is_valid/1
]).

calculate(StartByte, CommandBytes) ->
    <<0:1, S:7>> = StartByte,
    <<1:1, C1:7, 1:1, C2:7>> = CommandBytes,
    Checksum = S + C1 + C2,
    <<1:1, Checksum:7>>.

calculate_test() ->
    <<161>> = calculate(<<1>>, <<160, 128>>).

calculate_modulo_test() ->
    <<134>> = calculate(<<0>>, <<210, 180>>).

is_valid(<<0:1, S:7, 1:1, Sum:7, 1:1, C1:7, 1:1, C2:7>>) -> Sum =:= (S + C1 + C2) rem 128;
is_valid(<<_/binary>>) -> false.

valid_checksum_test() ->
    true = is_valid(<<1, 161, 160, 128>>).

checksum_modulo_test() ->
    true = is_valid(<<0, 134, 210, 180>>).

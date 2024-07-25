-module(go_modem_checksum).

-export([
    calculate/2,
    is_valid/1
]).

calculate(StartByte, CommandBytes) ->
    <<0:1, S:7>> = StartByte,
    <<1:1, C1:7, 1:1, C2:7>> = CommandBytes,
    Checksum = S + C1 + C2,
    <<1:1, Checksum:7>>.

is_valid(<<0:1, S:7, 1:1, Sum:7, 1:1, C1:7, 1:1, C2:7>>) -> Sum =:= (S + C1 + C2) rem 128;
is_valid(<<_/binary>>) -> false.

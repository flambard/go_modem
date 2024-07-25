-module(go_modem_protocol).

-include_lib("eunit/include/eunit.hrl").
-include("go_modem.hrl").

-export([
    decode_message/1,
    encode_message/1
]).

decode_message(Message) ->
    case go_modem_checksum:is_valid(Message) of
        false ->
            bad_checksum;
        true ->
            <<0:6, R:1, S:1, 1:1, _Checksum:7, CommandBytes:2/binary>> = Message,
            {CommandBits, ValueBits} = split_command_bits(CommandBytes),
            Command = decode_command(CommandBits, ValueBits),
            #message{send_seq_id = S, recv_seq_id = R, command = Command}
    end.

split_command_bits(<<1:1, Command:3, R:1, V1:3, 1:1, V2:7>>) ->
    0 = R,
    ValueBits = <<V1:3, V2:7>>,
    {Command, ValueBits}.

decode_command(2#000, <<2#111_111_1111:10>>) ->
    ok;
decode_command(2#001, <<0:10>>) ->
    deny;
decode_command(2#010, <<0:10>>) ->
    new_game;
decode_command(2#011, <<1:1, ExtendedCommand:9>>) ->
    {query, is_extended_command_supported, ExtendedCommand};
decode_command(2#011, <<0:1, Query:9>>) ->
    {query, go_modem_query:decode_query(Query)};
decode_command(2#100, <<Answer:10>>) ->
    {answer, Answer};
decode_command(2#101, <<Player:1, Move:9>>) ->
    {move, bit_to_player(Player), bits_to_move(Move)};
decode_command(2#110, <<Moves:10>>) ->
    {take_back, Moves};
decode_command(2#111, <<MoreBytes:10>>) ->
    %% TODO: Support extended commands
    {extended, MoreBytes}.

bit_to_player(0) -> black;
bit_to_player(1) -> white.

player_to_bit(black) -> 0;
player_to_bit(white) -> 1.

bits_to_move(0) -> pass;
bits_to_move(_N) -> not_implemented.

move_to_bits(pass) -> 0;
move_to_bits(_) -> not_implemented.

encode_message(#message{send_seq_id = S, recv_seq_id = R, command = Command}) ->
    StartByte = <<0:6, R:1, S:1>>,
    CommandBytes = encode_command(Command),
    ChecksumByte = go_modem_checksum:calculate(StartByte, CommandBytes),
    [StartByte, ChecksumByte, CommandBytes].

encode_command(ok) ->
    <<2#1000_0111_1111_1111:16>>;
encode_command(deny) ->
    <<2#1001_0000_1000_0000:16>>;
encode_command(new_game) ->
    <<2#1010_0000_1000_0000:16>>;
encode_command({query, is_extended_command_supported, ExtendedCommand}) ->
    <<2#1011:4, (encode_command_value(<<1:1, ExtendedCommand:9>>))/bitstring>>;
encode_command({query, Query}) ->
    QueryBits = go_modem_query:encode_query(Query),
    <<2#1011:4, (encode_command_value(<<0:1, QueryBits:9>>))/bitstring>>;
encode_command({answer, AnswerBits}) ->
    <<2#1100:4, (encode_command_value(AnswerBits))/bitstring>>;
encode_command({move, Player, Move}) ->
    PlayerBit = player_to_bit(Player),
    MoveBits = move_to_bits(Move),
    <<2#1101:4, (encode_command_value(<<PlayerBit:1, MoveBits:9>>))/bitstring>>;
encode_command({take_back, Moves}) ->
    <<2#1110:4, (encode_command_value(Moves))/bitstring>>;
encode_command({extended, _ExtendedCommand}) ->
    %% TODO: Support encoding extended commands
    %% encode_extended_command(ExtendedCommand),
    MoreBytes = 0,
    <<2#1111:4, (encode_command_value(MoreBytes))/bitstring>>.

encode_command_value(<<V1:3, V2:7>>) ->
    <<0:1, V1:3, 1:1, V2:7>>;
encode_command_value(Value) when is_integer(Value) ->
    encode_command_value(<<Value:10>>).

encode_new_game_message_test() ->
    Message = #message{send_seq_id = 1, recv_seq_id = 0, command = new_game},
    IoData = encode_message(Message),
    <<1, 161, 160, 128>> = iolist_to_binary(IoData).

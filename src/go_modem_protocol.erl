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
            #message{sender_seq_id = S, receiver_seq_id = R, command = Command}
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
    {query, decode_query(Query)};
decode_command(2#100, <<Answer/binary>>) ->
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
move_to_bits(_) -> not_imeplemented.

decode_query(0) -> what_game;
decode_query(1) -> modem_buffer_size;
decode_query(2) -> protocol_version;
decode_query(3) -> how_many_stones_on_board;
decode_query(4) -> black_time_spent;
decode_query(5) -> white_time_spent;
decode_query(6) -> character_set;
decode_query(7) -> rules;
decode_query(8) -> handicap.

encode_query(what_game) -> 0;
encode_query(modem_buffer_size) -> 1;
encode_query(protocol_version) -> 2;
encode_query(how_many_stones_on_board) -> 3;
encode_query(black_time_spent) -> 4;
encode_query(white_time_spent) -> 5;
encode_query(character_set) -> 6;
encode_query(rules) -> 7;
encode_query(handicap) -> 8.

encode_message(#message{sender_seq_id = S, receiver_seq_id = R, command = Command}) ->
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
    QueryBits = encode_query(Query),
    <<2#1011:4, (encode_command_value(<<0:1, QueryBits:9>>))/bitstring>>;
encode_command({answer, _Answer}) ->
    %% TODO: Support answering queries
    %% encode_answer(Answer),
    AnswerBits = 0,
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
    Message = #message{sender_seq_id = 0, receiver_seq_id = 1, command = new_game},
    IoData = encode_message(Message),
    <<1, 161, 160, 128>> = iolist_to_binary(IoData).

%   1 in binary: 00000001
% 161 in binary: 10100001
% 160 in binary: 10100000
% 128 in binary: 10000000



% Start byte:
%   0000 0001
%          hy

% - h (his sequence bit) = 0
% - y (your sequence bit) = 1


% Checksum byte:
%   1010 0001
%    sss ssss

% - s (checksum) = 0100001


% Command bytes:
%   1010 0000 1000 0000
%    ccc rvvv  vvv vvvv

% - c (command) = 010 (NEWGAME)
% - r (reserved) = 0
% - v (value) = 0

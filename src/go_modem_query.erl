-module(go_modem_query).

-export([
    decode_query/1,
    encode_query/1,
    decode_answer/2,
    encode_answer/2
]).

decode_query(0) -> game;
decode_query(1) -> modem_buffer_size;
decode_query(2) -> protocol_version;
decode_query(3) -> stones_on_board;
decode_query(4) -> black_time_spent;
decode_query(5) -> white_time_spent;
decode_query(6) -> character_set;
decode_query(7) -> rules;
decode_query(8) -> handicap;
decode_query(9) -> board_size;
decode_query(10) -> time_limit;
decode_query(11) -> color;
decode_query(12) -> who.

encode_query(game) -> 0;
encode_query(modem_buffer_size) -> 1;
encode_query(protocol_version) -> 2;
encode_query(stones_on_board) -> 3;
encode_query(black_time_spent) -> 4;
encode_query(white_time_spent) -> 5;
encode_query(character_set) -> 6;
encode_query(rules) -> 7;
encode_query(handicap) -> 8;
encode_query(board_size) -> 9;
encode_query(time_limit) -> 10;
encode_query(color) -> 11;
encode_query(who) -> 12.

decode_answer(game, Answer) -> decode_game(Answer);
decode_answer(modem_buffer_size, Answer) -> decode_modem_buffer_size(Answer);
decode_answer(protocol_version, 0) -> 0;
decode_answer(stones_on_board, Answer) -> Answer;
decode_answer(black_time_spent, Answer) -> Answer;
decode_answer(white_time_spent, Answer) -> Answer;
decode_answer(character_set, Answer) -> decode_character_set(Answer);
decode_answer(rules, Answer) -> decode_rules(Answer);
decode_answer(handicap, Answer) -> decode_handicap(Answer);
decode_answer(board_size, Answer) -> decode_board_size(Answer);
decode_answer(time_limit, Answer) -> decode_time_limit(Answer);
decode_answer(color, Answer) -> decode_color(Answer);
decode_answer(who, Answer) -> decode_who(Answer).

encode_answer(game, Answer) -> encode_game(Answer);
encode_answer(modem_buffer_size, Answer) -> encode_modem_buffer_size(Answer);
encode_answer(protocol_version, 0) -> 0;
encode_answer(stones_on_board, Answer) -> Answer;
encode_answer(black_time_spent, Answer) -> Answer;
encode_answer(white_time_spent, Answer) -> Answer;
encode_answer(character_set, Answer) -> encode_character_set(Answer);
encode_answer(rules, Answer) -> encode_rules(Answer);
encode_answer(handicap, Answer) -> encode_handicap(Answer);
encode_answer(board_size, Answer) -> encode_board_size(Answer);
encode_answer(time_limit, Answer) -> encode_time_limit(Answer);
encode_answer(color, Answer) -> encode_color(Answer);
encode_answer(who, Answer) -> encode_who(Answer).

%% ----------------------------------------------------------------------------

decode_game(0) -> unknown;
decode_game(1) -> go;
decode_game(2) -> chess;
decode_game(3) -> othello.

encode_game(unknown) -> 0;
encode_game(go) -> 1;
encode_game(chess) -> 2;
encode_game(othello) -> 3.

decode_modem_buffer_size(N) -> N * 16 + 4.

encode_modem_buffer_size(N) -> (N - 4) div 16.

decode_character_set(0) -> unknown;
decode_character_set(1) -> ascii;
decode_character_set(2) -> japanese.

encode_character_set(unknown) -> 0;
encode_character_set(ascii) -> 1;
encode_character_set(japanese) -> 2.

decode_rules(0) -> unknown;
decode_rules(1) -> japanese;
decode_rules(2) -> ing_chinese.

encode_rules(unknown) -> 0;
encode_rules(japanese) -> 1;
encode_rules(ing_chinese) -> 2.

decode_handicap(0) -> unknown;
decode_handicap(1) -> even;
decode_handicap(N) -> N.

encode_handicap(unknown) -> 0;
encode_handicap(even) -> 1;
encode_handicap(N) when is_integer(N) -> N.

decode_board_size(0) -> unknown;
decode_board_size(N) -> N.

encode_board_size(unknown) -> 0;
encode_board_size(N) when is_integer(N) -> N.

decode_time_limit(0) -> unknown;
decode_time_limit(N) -> N.

encode_time_limit(unknown) -> 0;
encode_time_limit(N) when is_integer(N) -> N.

decode_color(0) -> unknown;
decode_color(1) -> white;
decode_color(2) -> black.

encode_color(unknown) -> 0;
encode_color(white) -> 1;
encode_color(black) -> 2.

decode_who(0) -> unknown;
decode_who(1) -> nemesis;
decode_who(2) -> many_faces_of_go;
decode_who(3) -> smart_go_board;
decode_who(4) -> goliath;
decode_who(5) -> go_intellect;
decode_who(6) -> star_of_poland.

encode_who(unknown) -> 0;
encode_who(nemesis) -> 1;
encode_who(many_faces_of_go) -> 2;
encode_who(smart_go_board) -> 3;
encode_who(goliath) -> 4;
encode_who(go_intellect) -> 5;
encode_who(star_of_poland) -> 6.

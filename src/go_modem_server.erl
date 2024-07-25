-module(go_modem_server).

-type server() :: term().
-type player() :: black | white.
%% TODO: better type spec for moves
-type move() :: term().

-type query() ::
    game
    | modem_buffer_size
    | protocol_version
    | stones_on_board
    | black_time_spent
    | white_time_spent
    | character_set
    | rules
    | handicap
    | board_size
    | time_limit
    | color
    | who.

%% TODO: better type spec for answers
-type answer() :: term().

-callback handle_new_game(server()) -> ok | deny.
-callback handle_move(server(), player(), move()) -> ok | deny.
-callback handle_query(server(), query()) -> answer().

%% Optional callback example
%
% -optional_callbacks([
%     decode_response_values/1
% ]).
%

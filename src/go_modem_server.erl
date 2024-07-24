-module(go_modem_server).

-type server() :: term().
-type player() :: black | white.

-callback new_game(server()) -> ok | deny.
-callback move(server(), player(), Move :: term()) -> ok | deny.

% -callback command_name() -> CommandName :: binary().

% -callback encode_command_arguments(Command :: command()) -> EncodedArguments :: [iodata()].

% -callback decode_response_values(EncodedResponseLines :: [binary()]) ->
%     ResponseValues :: response_values().

% -optional_callbacks([
%     decode_response_values/1
% ]).

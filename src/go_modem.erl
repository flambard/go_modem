-module(go_modem).

-export([
    start_connection/3,
    query/2
]).

-type query() :: atom().
-type answer() :: term().

-spec start_connection(IoDevice :: term(), Server :: pid(), ServerMod :: module()) ->
    {ok, pid()} | {error, Reason :: term()}.

start_connection(IoDevice, Server, ServerMod) ->
    go_modem_sup:start_connection(IoDevice, Server, ServerMod).

-spec query(Connection :: pid(), Query :: query()) -> {ok, answer()} | {error, term()}.

query(Connection, Query) ->
    go_modem_connection:send_command(Connection, {query, Query}).

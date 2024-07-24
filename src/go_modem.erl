-module(go_modem).

-export([start_connection/3]).

start_connection(IoDevice, Server, ServerMod) ->
    go_modem_sup:start_connection(IoDevice, Server, ServerMod).

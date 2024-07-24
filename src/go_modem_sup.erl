-module(go_modem_sup).
-behaviour(supervisor).

%% API
-export([
    start_link/0,
    start_connection/3
]).

%% supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%
%%% API
%%%

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start_connection(IoDevice, Server, ServerMod) ->
    supervisor:start_child(?SERVER, [IoDevice, Server, ServerMod]).

%%%
%%% supervisor callbacks
%%%

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init([]) ->
    SupFlags = #{
        strategy => simple_one_for_one,
        intensity => 0,
        period => 1
    },
    ChildSpecs = [
        #{
            id => go_modem_connection,
            start => {go_modem_connection, start_link, []}
        }
    ],
    {ok, {SupFlags, ChildSpecs}}.

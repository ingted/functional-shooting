-module(shootingserver_cowboy).
-export([start/0]).

start() ->
    ok = application:start(ranch),
    ok = application:start(cowboy),
    ok = application:start(jiffy),
    Dispatch = [
        {'_', [
            {[<<"player">>], player_handler, []}
        ]}
    ],
    cowboy:start_http(shooting_listener, 100, [{port, 19860}],
        [{dispatch, Dispatch}]
    ),
    ok.


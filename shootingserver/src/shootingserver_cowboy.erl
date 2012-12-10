-module(shootingserver_cowboy).
-export([start/0]).

start() ->
    %%application:start(compiler),
    %%application:start(syntax_tools),
    %%{ok, [{handlers, LConfig}]} = application:get_env(shootingserver, lager),
    %%application:load(lager),
    %%application:set_env(lager,handlers, LConfig),
    %%application:start(lager),

    application:start(ranch),
    application:start(cowboy),
    application:start(jiffy),
    Dispatch = [
        {'_', [
            {[<<"shooting">>], shooting_handler, []}
        ]}
    ],
    cowboy:start_http(shooting_listener, 100, [{port, 19860}],
        [{dispatch, Dispatch}]
    ),
    ok.


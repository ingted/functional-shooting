-module(player_handler).
-behaviour(cowboy_http_handler).
-behaviour(cowboy_websocket_handler).
-export([init/3, handle/2, terminate/2]).

-export([websocket_init/3, websocket_handle/3,
         websocket_info/3, websocket_terminate/3]).

init({tcp, http}, _Req, _Opts) ->
    {upgrade, protocol, cowboy_websocket}.

handle(_Req, State) ->
    {ok, Req2} = cowboy_req:reply(404, [
        {'Content-Type', <<"text/html">>}
    ]),
    {ok, Req2, State}.

terminate(_Req, _State) ->
    ok.

websocket_init(_Any, Req, _Opts) ->
    Interval = round(1000 / 30),
    timer:send_interval(Interval, tick),
    Req2 = cowboy_req:compact(Req),
    {ok, Req2, undefined, hibernate}.

websocket_handle({text, Msg}, Req, _State) ->
    {[{_X, X}, {_Y, Y}]} = jiffy:decode(Msg),
    {ok, Req, {X, Y}};
websocket_handle(_Any, Req, State) ->
    {ok, Req, State}.

websocket_info(tick, Req, {X, Y}) ->
    Data = jiffy:encode({[{x, X}, {y, Y}]}),
    {reply, {text, Data}, Req, {X, Y}, hibernate};
websocket_info(_Info, Req, State) ->
    {ok, Req, State, hibernate}.

websocket_terminate(_Reason, _Req, _State) ->
    ok.

%%%-------------------------------------------------------------------
%% @doc stream public API
%% @end
%%%-------------------------------------------------------------------

-module(stream_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    Dispatch = cowboy_router:compile([{'_', [{"/", stream_server, []}]}]),
    {ok, _} = cowboy:start_http(stream_http_listener,
                                100,
                                [{port, 8080}],
                                [{env, [{dispatch, Dispatch}]}]),
    stream_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================

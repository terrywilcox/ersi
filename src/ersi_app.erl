%%%-------------------------------------------------------------------
%% @doc ersi public API
%% @end
%%%-------------------------------------------------------------------

-module(ersi_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    ersi_sup:start_link().

stop(_State) ->
    ok.

%% internal functions

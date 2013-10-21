-module(ehbase_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    application:start(lager),
    application:start(pooler),
    ehbase_sup:start_link().

stop(_State) ->
    ok.

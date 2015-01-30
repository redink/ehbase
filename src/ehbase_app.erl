-module(ehbase_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).
-export([start_profiling/0, profile_output/0]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    start_profiling(),
    lager:start(),
    application:start(sync),
    application:start(pooler),
    ehbase_sup:start_link().

stop(_State) ->
    ok.

-spec start_profiling() -> profiling | not_profiling | {error, any()}.
start_profiling() ->
    case application:get_env(ehbase, profile) of
    {ok, true} ->
        {ok, _} = eprof:start(),
        eprof:start_profiling([erlang:self()]);
    _ ->
        not_profiling
    end.

-spec profile_output() -> ok.
profile_output() ->
    eprof:stop_profiling(),
    eprof:log("ehbase.procs.profile"),
    eprof:analyze(procs),
    eprof:log("ehbase.total.profile"),
    eprof:analyze(total).

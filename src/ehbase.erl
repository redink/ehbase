-module(ehbase).

-compile(export_all).

get(TableName, Row, Column, Timeout) ->
    PidForUse = pooler:take_member(hbase_thrift),
    ExecuteResult =
        case catch gen_server:call(PidForUse,
                                   {get, [TableName, Row, Column, dict:new()]},
                                   Timeout) of
            {ok, Result} ->
                pooler:return_member(hbase_thrift, PidForUse),
                {true, Result};
            _ ->
                restart_pid(PidForUse),
                {false, []}
        end,
    ExecuteResult.

restart_pid(Pid) ->
    Pid.

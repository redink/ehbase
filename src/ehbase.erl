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
            _Any ->
                reset_thrift_conn(PidForUse),
                pooler:return_member(hbase_thrift, PidForUse),
                {false, []}
        end,
    ExecuteResult.

reset_thrift_conn(Pid) ->
    gen_server:call(Pid, {reset_connection}).

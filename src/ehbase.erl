-module(ehbase).

-compile(export_all).

get(TableName, Row, Column, Timeout) ->

    case pooler:take_member(hbase_thrift, Timeout) of
        error_no_members ->
            {false, []};
        PidForUse when erlang:is_pid(PidForUse) ->
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
            ExecuteResult;
        _ ->
            {false, []}
    end.

reset_thrift_conn(Pid) ->
    gen_server:call(Pid, {reset_connection}).

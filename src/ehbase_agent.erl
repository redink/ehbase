%%%-------------------------------------------------------------------
%%% @author redink <tao@redink.biz>
%%% @copyright (C) 2013, redink
%%% @doc
%%%
%%% @end
%%% Created : 19 Oct 2013 by redink <tao@redink.biz>
%%%-------------------------------------------------------------------
-module(ehbase_agent).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
     terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, {hbase_thrift_server,
                hbase_thrift_ip,
                hbase_thrift_port,
                hbase_thrift_params,
                hbase_thrift_connection}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link(?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    erlang:process_flag(trap_exit, true),
    Hbase_Thrift_Server = 
        get_env_default(ehbase, hbase_thrift_server, hbase_thrift),
    Hbase_Thrift_IP     = 
        get_env_default(ehbase, hbase_thrift_ip,     "localhost"),
    Hbase_Thrift_Port   = 
        get_env_default(ehbase, hbase_thrift_port,   9090),
    Hbase_Thrift_Params =
        get_env_default(ehbase, hbase_thrift_params, []),
    case catch thrift_client_util:new(Hbase_Thrift_IP,
                                      Hbase_Thrift_Port,
                                      Hbase_Thrift_Server, 
                                      Hbase_Thrift_Params) of
        {ok, Connection} ->
            lager:debug("ehbase agent started success"),
            {ok, #state{hbase_thrift_server     = Hbase_Thrift_Server,
                        hbase_thrift_ip         = Hbase_Thrift_IP,
                        hbase_thrift_port       = Hbase_Thrift_Port,
                        hbase_thrift_params     = Hbase_Thrift_Params,
                        hbase_thrift_connection = Connection}};
        _Any ->
            lager:error("ehbase agent start error, info ~p~n", [_Any]),
            {stop, error}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({getTableNames}, _, 
        #state{hbase_thrift_connection = Connection} = State) ->
    {_, R} = (catch thrift_client:call(Connection, getTableNames, [])),
    {reply, R, State};

handle_call({getColumnDescriptors, TableName}, _,
        #state{hbase_thrift_connection = Connection} = State) ->
    {_, R} = (catch thrift_client:call(Connection, 
                                        getColumnDescriptors, 
                                        [TableName])),
    {reply, R, State};

handle_call({getTableRegions, TableName}, _, 
        #state{hbase_thrift_connection = Connection} = State) ->
    {_, R} = (catch thrift_client:call(Connection,
                                        getTableRegions,
                                        [TableName])),
    {reply, R, State};

handle_call({Function_Name, Params}, _, 
        #state{hbase_thrift_connection = Connection} = State) ->
    case erlang:is_atom(Function_Name) andalso erlang:is_list(Params) of
        true ->
            {NewConnection, Result} = (catch thrift_client:call(Connection,
                                                Function_Name, 
                                                Params)),
            {reply, Result, State#state{hbase_thrift_connection = NewConnection}};
        false ->
            {reply, "function or params error", State}
    end;

handle_call({reset_connection}, _, 
        #state{hbase_thrift_ip         = Hbase_Thrift_IP,
               hbase_thrift_port       = Hbase_Thrift_Port,
               hbase_thrift_server     = Hbase_Thrift_Server,
               hbase_thrift_params     = Hbase_Thrift_Params,
               hbase_thrift_connection = Connection} = State) ->
    catch thrift_client:close(Connection),
    case catch thrift_client_util:new(Hbase_Thrift_IP,
                                      Hbase_Thrift_Port,
                                      Hbase_Thrift_Server, 
                                      Hbase_Thrift_Params) of
        {ok, NewConnection} ->
            lager:debug("reset connection successed"),
            {reply, ok, #state{hbase_thrift_connection = NewConnection}};
        _Any ->
            lager:error("ehbase agent start error, info ~p~n", [_Any]),
            {stop, error}
    end;

handle_call(_Request, _From, State) ->
    Reply = unsupport,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast({Function_Name, Params}, 
        #state{hbase_thrift_connection = Connection} = State) ->
    case erlang:is_atom(Function_Name) andalso erlang:is_list(Params) of
        true ->
            {NewConnection, Result} = (catch thrift_client:call(Connection,
                                                Function_Name, 
                                                Params)),
            {reply, Result, State#state{hbase_thrift_connection = NewConnection}};
        false ->
            {reply, "function or params error", State}
    end;

handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(Reason, State) ->
    lager:error("hbase agent terminate, reason info ~p", [Reason]),
    catch thrift_client:close(State#state.hbase_thrift_connection),
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @doc get application env, if undefined will return Default
%%
-spec get_env_default(atom(), atom(), term()) -> term().
get_env_default(App, Key, Default) ->
    case application:get_env(App, Key) of
        undefined ->
            Default;
        {ok, _Val} ->
            _Val
    end.

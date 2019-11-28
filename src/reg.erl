-module(reg).
-export([
    register/2,
    register/3,
    unregister/2,
    lookup/2,
    fold/3
]).

-export([
    start_link/2,
    child_spec/2
]).

-export([
    whereis_name/1,
    register_name/2,
    send/2,
    unregister_name/1
]).

-behaviour(gen_server).
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2
]).

-export_type([
    registry/0,
    kind/0,
    key/0,
    value/0,
    listener/0
]).

-type registry() :: atom().
-type kind() :: unique | duplicate.
-type key() :: term().
-type value() :: term().
-type listener() :: atom() | {module(), atom(), [term()]} | fun((registry(), listener_event()) -> any()).
-type listener_event() :: {register, pid(), key(), value()} | {unregister, pid(), key()}.

-record(state, {
    name       :: atom(),
    kind       :: kind(),
    listeners  :: [listener()],
    keys_table :: ets:tab(), % {key(), pid(), value()}    type: set | duplicate_bag
    pids_table :: ets:tab()  % {pid(), key()}             type: duplicate_bag
}).

-include_lib("stdlib/include/ms_transform.hrl").


-spec register(registry(), key()) -> ok | {error, Error} when
    Error :: {already_registered, pid()}.
register(Registry, Key) ->
    register(Registry, Key, undefined).


-spec register(registry(), key(), value()) -> ok | {error, Error} when
    Error :: {already_registered, pid()}.
register(Registry, Key, Value) ->
    gen_server:call(Registry, {register, self(), Key, Value}, infinity).


-spec unregister(registry(), key()) -> ok | {error, Error} when
    Error :: not_registered.
unregister(Registry, Key) ->
    gen_server:call(Registry, {unregister, self(), Key}, infinity).


-spec lookup(registry(), key()) -> [{pid(), value()}].
lookup(Registry, Key) ->
    [{Pid, Value} || {_Key, Pid, Value} <- ets:lookup(Registry, Key)].


-spec fold(registry(), Fun, Acc) -> Acc when
    Fun :: fun((pid(), key(), value(), Acc) -> Acc),
    Acc :: term().
fold(Registry, Fun, Acc0) ->
    ets:foldl(fun ({Key, Pid, Value}, Acc) ->
        Fun(Pid, Key, Value, Acc)
    end, Acc0, Registry).


-spec start_link(registry(), options()) -> {ok, pid()}.
-type options() :: #{
    kind => kind(),
    listeners => [listener()]
}.
start_link(Name, Options) ->
    gen_server:start_link({local, Name}, ?MODULE, [Name, Options], []).


-spec child_spec(registry(), options())-> supervisor:child_spec().
child_spec(Name, Options) ->
    #{
        id => Name,
        start => {?MODULE, start_link, [Name, Options]},
        type => worker
    }.


%%====================================================================
%% Via callbacks
%%====================================================================

%% @hidden
whereis_name({Registry, Key}) ->
    whereis_name(Registry, Key);

whereis_name({Registry, Key, _Value}) ->
    whereis_name(Registry, Key).


%% @private
whereis_name(Registry, Key) ->
    case ets:info(Registry, type) of
        set ->
            case lookup(Registry, Key) of
                [{Pid, _}] ->
                    case is_process_alive(Pid) of
                        true -> Pid;
                        false -> undefined
                    end;
                [] ->
                    undefined
            end;
        _ ->
            erlang:error(badarg, [Registry, Key])
    end.


%% @hidden
register_name({Registry, Key}, Pid) ->
    register_name(Registry, Key, undefined, Pid);

register_name({Registry, Key, Value}, Pid) ->
    register_name(Registry, Key, Value, Pid).


%% @private
register_name(Registry, Key, Value, Pid) when Pid == self() ->
    case register(Registry, Key, Value) of
        ok -> yes;
        {error, _} -> no
    end.


%% @hidden
send({Registry, Key}, Message) ->
    case lookup(Registry, Key) of
        [{Pid, _}] -> erlang:send(Pid, Message);
        [] -> erlang:error(badarg, [{Registry, Key}, Message])
    end.


%% @hidden
unregister_name({Registry, Key}) ->
    unregister(Registry, Key).


%%====================================================================
%% gen_server callbacks
%%====================================================================

%% @hidden
init([Name, Options]) ->
    % The registering processes link against this server
    _ = erlang:process_flag(trap_exit, true),
    Kind = maps:get(kind, Options, unique),
    Listeners = maps:get(listeners, Options, []),
    KeysTable = ets:new(Name, [
        case Kind of
            unique -> set;
            duplicate -> duplicate_bag
        end,
        protected,
        named_table,
        {read_concurrency, true}
    ]),
    PidsTable = ets:new(reg_server_pids, [
        duplicate_bag,
        private
    ]),
    {ok, #state{
        name = Name,
        kind = Kind,
        listeners = Listeners,
        keys_table = KeysTable,
        pids_table = PidsTable
    }}.


%% @hidden
handle_call({register, Pid, Key, Value}, _From, State) ->
    #state{name = Name, kind = Kind, listeners = Listeners, keys_table = KeysTable, pids_table = PidsTable} = State,
    case register_key(Kind, KeysTable, Pid, Key, Value) of
        ok ->
            erlang:link(Pid),
            true = ets:insert(PidsTable, {Pid, Key}),
            notify_listeners(Name, {register, Pid, Key, Value}, Listeners),
            {reply, ok, State};
        {error, {already_registered, _Pid}} = Error ->
            {reply, Error, State}
    end;

handle_call({unregister, Pid, Key}, _From, State) ->
    #state{name = Name, kind = Kind, listeners = Listeners, keys_table = KeysTable, pids_table = PidsTable} = State,
    case unregister_key(Kind, KeysTable, Pid, Key) of
        ok ->
            notify_listeners(Name, {unregister, Pid, Key}, Listeners),
            true = ets:delete_object(PidsTable, {Pid, Key}),
            case ets:member(PidsTable, Pid) of
                true ->
                    {reply, ok, State};
                false ->
                    erlang:unlink(Pid),
                    {reply, ok, State}
            end;
        {error, _} = Error ->
            {reply, Error, State}
    end;

handle_call(Request, _From, State) ->
    {reply, {error, {bad_request, Request}}, State}.


%% @hidden
handle_cast(_Request, State) ->
    {noreply, State}.


%% @hidden
handle_info({'EXIT', Pid, _Reason}, State) ->
    #state{name = Name, kind = Kind, listeners = Listeners, keys_table = KeysTable, pids_table = PidsTable} = State,
    Entries = ets:take(PidsTable, Pid),
    lists:foreach(fun ({_Pid, Key}) ->
        ok = unregister_key(Kind, KeysTable, Pid, Key),
        notify_listeners(Name, {unregister, Pid, Key}, Listeners)
    end, Entries),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.


%%====================================================================
%% helpers
%%====================================================================


%% @private
register_key(duplicate, KeysTable, Pid, Key, Value) ->
    true = ets:insert(KeysTable, {Key, Pid, Value}),
    ok;

register_key(unique, KeysTable, Pid, Key, Value) ->
    case ets:insert_new(KeysTable, {Key, Pid, Value}) of
        true ->
            ok;
        false ->
            case ets:lookup(KeysTable, Key) of
                [{Key, ExistingPid, _ExistingValue} = ExistingEntry] ->
                    case is_process_alive(ExistingPid) of
                        true ->
                            {error, {already_registered, ExistingPid}};
                        false ->
                            true = ets:delete_object(KeysTable, ExistingEntry),
                            register_key(unique, KeysTable, Pid, Key, Value)
                    end;
                [] ->
                    %% This will never happen while we are serializing the registration requests
                    register_key(unique, KeysTable, Pid, Key, Value)
            end
    end.


unregister_key(_Kind, KeysTable, Pid, Key) ->
    Count = ets:select_delete(KeysTable, ets:fun2ms(fun
        ({K, P, _}) when K =:= Key, P =:= Pid ->
            true
    end)),
    if
        Count > 0 -> ok;
        Count =:= 0 -> {error, not_registered}
    end.


notify_listeners(Name, Event, [Listener | Listeners]) ->
    _ = notify_listener(Name, Event, Listener),
    notify_listeners(Name, Event, Listeners);

notify_listeners(_Name, _Event, []) ->
    ok.


notify_listener(Name, Event, Listener) when is_function(Listener, 2) ->
    spawn(fun () ->
        Listener(Name, Event)
    end);

notify_listener(Name, Event, {Module, Fun, Args}) when is_atom(Module), is_atom(Fun), is_list(Args) ->
    spawn(Module, Fun, Args ++ [Name, Event]);

notify_listener(Name, Event, Listener) when is_atom(Listener) ->
    Listener ! {reg, Name, Event}.

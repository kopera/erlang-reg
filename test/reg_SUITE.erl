-module(reg_SUITE).

-include_lib("common_test/include/ct.hrl").
-compile(export_all).
-compile(nowarn_export_all).


suite() ->
    [{timetrap, {seconds, 10}}].


all() ->
    [
        {group, unique},
        {group, duplicate}
    ].

groups() ->
    [
        {unique, [parallel], [
            has_unique_registrations,
            has_unique_registrations_across_processes,
            unregister_process_with_key,
            unregister_process_with_tricky_key
        ]},
        {duplicate, [parallel], [
            has_duplicated_registrations,
            unregister_process_with_key,
            unregister_process_with_tricky_key
        ]}
    ].


init_per_group(Group, Config) ->
    [{reg_kind, Group} | Config].


end_per_group(_, Config) ->
    Config.


init_per_testcase(Case, Config) ->
    Name = Case,
    reg:start_link(Name, #{
        kind => ?config(reg_kind, Config)
    }),
    [{reg_name, Name} | Config].

end_per_testcase(_, Config) ->
    Config.


%% ======================================================================-
%% test cases
%% ======================================================================-

has_unique_registrations(Config) ->
    Registry = ?config(reg_name, Config),
    SelfPid = self(),
    ok = reg:register(Registry, "a", value),
    [{SelfPid, value}] = reg:lookup(Registry, "a"),

    {error, {already_registered, SelfPid}} = reg:register(Registry, "a", value),
    [{SelfPid, value}] = reg:lookup(Registry, "a"),

    ok = reg:register(Registry, "b", value),
    [{SelfPid, value}] = reg:lookup(Registry, "a"),
    [{SelfPid, value}] = reg:lookup(Registry, "b").


has_unique_registrations_across_processes(Config) ->
    Registry = ?config(reg_name, Config),
    SelfPid = self(),
    OtherPid = spawn_link(fun () ->
        ok = reg:register(Registry, "a", value),
        SelfPid ! registered,
        receive
            terminate ->
                ok = reg:unregister(Registry, "a"),
                SelfPid ! terminated
        end
    end),

    receive
        registered -> ok
    end,

    {error, {already_registered, OtherPid}} = reg:register(Registry, "a", value),
    [{OtherPid, value}] = reg:lookup(Registry, "a"),

    OtherPid ! terminate,

    receive
        terminated -> ok
    end,

    ok = reg:register(Registry, "a", value),
    [{SelfPid, value}] = reg:lookup(Registry, "a").


has_duplicated_registrations(Config) ->
    Registry = ?config(reg_name, Config),
    SelfPid = self(),
    ok = reg:register(Registry, "a", value1),
    [{SelfPid, value1}] = reg:lookup(Registry, "a"),

    ok= reg:register(Registry, "a", value2),
    [{SelfPid, value1}, {SelfPid, value2}] = lists:sort(reg:lookup(Registry, "a")),

    ok = reg:register(Registry, "b", value),
    [{SelfPid, value1}, {SelfPid, value2}] = lists:sort(reg:lookup(Registry, "a")),
    [{SelfPid, value}] = reg:lookup(Registry, "b").


unregister_process_with_key(Config) ->
    Registry = ?config(reg_name, Config),
    SelfPid = self(),
    {error, not_registered} = reg:unregister(Registry, "a"),
    {error, not_registered} = reg:unregister(Registry, "b"),

    ok = reg:register(Registry, "a", value),
    ok = reg:register(Registry, "b", value),

    [{SelfPid, value}] = reg:lookup(Registry, "a"),
    [{SelfPid, value}] = reg:lookup(Registry, "b"),

    ok = reg:unregister(Registry, "b"),
    [{SelfPid, value}] = reg:lookup(Registry, "a"),
    [] = reg:lookup(Registry, "b"),

    ok = reg:unregister(Registry, "a"),
    [] = reg:lookup(Registry, "a"),
    [] = reg:lookup(Registry, "b").


unregister_process_with_tricky_key(Config) ->
    Registry = ?config(reg_name, Config),
    SelfPid = self(),
    {error, not_registered} = reg:unregister(Registry, '$1'),
    {error, not_registered} = reg:unregister(Registry, '_'),

    ok = reg:register(Registry, '$1', value),
    ok = reg:register(Registry, '_', value),

    [{SelfPid, value}] = reg:lookup(Registry, '$1'),
    [{SelfPid, value}] = reg:lookup(Registry, '_'),

    ok = reg:unregister(Registry, '_'),
    [{SelfPid, value}] = reg:lookup(Registry, '$1'),
    [] = reg:lookup(Registry, '_'),

    ok = reg:unregister(Registry, '$1'),
    [] = reg:lookup(Registry, '$1'),
    [] = reg:lookup(Registry, '_').

# reg

An Erlang process registry based on/inspired by the Elixir process registry.

This registry allows for the lookup of processes based on a given key. The
registry comes in two flavors/kinds: `unique` and `duplicate`. When a registry
is started as `unique` registry, only one process is allowed to register using
the given key, that is a given key will point to 0 or 1 processes.

# Setup

You need to add `reg` as a dependency to your project. If you are using `rebar3`,
you can add the following to your `rebar.config`:

```erlang
{deps, [
    {reg, "0.1.0"}
]}.
```

Also ensure that `reg` is added as a dependency to your application, by updating
your `.app.src` file:

```erlang
{application, my_app, [

    {applications, [
        kernel,
        stdlib,

        reg  % <- You need this in your applications list
    ]}
]}.
```

# Usage

## Starting a registry

You can start one or more registries in your application, it is recommended to start
`reg` under your supervision tree:

```erlang
init([]) ->
    Flags = #{strategy => rest_for_one},
    Children = [
        reg:child_spec(my_registry, #{
            kind => duplicate % <- This should be set to `unique` for unique registries
        }),
        % ...
    ],
    {ok, {Flags, Children}}.
```

## Registering a process
To register a process with the key `key1` and the value `value`, you just need to
call `register/3` from the registering process

```erlang
> reg:register(my_registry, key1, value).
ok
```

## Looking up processes

To find all processes registered under the key `key1`:

```erlang
> reg:lookup(my_registry, key1).
[{<0.145.0>, value}]
```

## Unregistering
A process can unregister by calling the `reg:unregister/2` function. A process is
also automatically unregistered when it terminates.

```erlang
> reg:unregister(my_registry, key1).
ok
```
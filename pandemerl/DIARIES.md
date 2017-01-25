# Project setup

Install [rebar3](http://www.rebar3.org/docs/basic-usage)

Create an application skeleton

```bash
$ rebar3 new app pandemerl
```

Add [cowboy](https://github.com/ninenines/cowboy) dependency in `rebar.config`

```
{erl_opts, [debug_info]}.
{deps, [
        {cowboy, {git, "git://github.com/ninenines/cowboy.git"}}
       ]}.
{plugins, [rebar3_run]}.
```

*Warning*: one uses the `master` branch for cowboy, mainly due to API changes that are in the documentation but not in a release version yet.

Grab the dependency before being offline

```
$ rebar3 compile
``` 

Add cowboy in the list of application in `pandemerl.app.src`
```
{applications,
   [kernel,
    stdlib,
    cowboy
   ]},
```

# Registry

Terminal 1

```
$ rebar3 do clean, compile, shell --sname term1
...
Erlang/OTP 19 [erts-8.1] [source] [64-bit] [smp:4:4] [async-threads:0] [hipe] [kernel-poll:false]

Eshell V8.1  (abort with ^G)
(term1@Mentem)1> registry_srv:start_link().
```

Terminal 2

```
$ rebar3 do shell --sname term2
...
Erlang/OTP 19 [erts-8.1] [source] [64-bit] [smp:4:4] [async-threads:0] [hipe] [kernel-poll:false]

Eshell V8.1  (abort with ^G)
(term2@Mentem)1> net_kernel:connect_node('term1@Mentem').
```
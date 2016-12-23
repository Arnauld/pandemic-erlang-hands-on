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


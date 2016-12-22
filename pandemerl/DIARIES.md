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
        {cowboy, {git, "https://github.com/ninenines/cowboy", {tag, "1.0.4"}}}
]}.
{plugins, [rebar3_run]}.
```

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


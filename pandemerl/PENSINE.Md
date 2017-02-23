```erlang
infection_level(City, Disease) ->
  City ! {infection_levels, self(), Disease},
  receive
    {infection_levels, Level} ->
      Level
  end.

loop(State) ->
  receive
    {infection_levels, From, Disease} ->
      Level = city:infection_level(State, Disease),
      From ! {infection_levels, Level},
      loop(State)
  end.
```
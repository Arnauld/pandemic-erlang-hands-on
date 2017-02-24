# 1.Raw Erlang

`city.erl` + `city_tests.erl`

* atom
* tuple
* list
* number
* immutability
* pattern matching
* recursion/tail recursion
* module

## Extra

* proplists

## Cheat sheet

```erlang
London = {london, []}.

{Name, _} = London.
Name = london.

{_, Levels} = London.
[] = Levels.

NewLevel = Level + 1.
Level = Level + 1. ???

case Level of
  0 ->
    io:format("No yet infected~n", []);
    
  1 ->
    io:format("Infection started!~n", []);
    
  N ->
    io:format("Infection level ~p~n", [])
end.

NewLevels1 = [{blue, 1} | Levels],
NewLevels2 = [{red, 3} | NewLevels1],
[Head | Tail] = NewLevels2,
[{red, RedLevel} | Tail] = NewLevels2,
3 = RedLevel.
```

```erlang
-module(name).
-export([new/1]).

new(Name) ->
    {ok, Name}.
```

```erlang
-module(calculator).
-export([add/2, div/2]).

add(A,B) -> A + B.

div(A,0) -> div_by_zero;
div(A,1) -> A;
div(A,B) -> A / B.
```

# 2.Process

`city_proc.erl` + `city_proc_tests.erl`

* `error_logger`
* module `?MODULE`
* MFA
* process
    * spaw, receive
    * `self()`
    
     
## Extra

* error_logger
* guard

## Cheat sheet

```erlang
-module(calculator).

-export([start/0, add/2, div/2]).

%% internal loop
-export([calc/0]).

start() ->
  Pid = spawn(?MODULE, calc, []),
  Pid.
  
add(Calc, A, B) when is_pid(Calc) ->
  Calc ! {add, A, B}.

div(Calc, A, B) when is_pid(Calc) ->
  Calc ! {div, A, B}.

calc() ->
  receive
    {add, A, B} ->
      io:format("ADD(~p,~p) = ~p~n", [A, B, A + B]);
      
    {div, A, 0} ->
      io:format("DIV(~p,~p) ~p~n", [A, B, div_by_zero]);
    
    {div, A, B} ->
      io:format("DIV(~p,~p) ~p~n", [A / B])
      
  end.
```

# 2.Processes and non-blocking way

* register
* unregister try/after
* record to the rescue

## Cheat sheet

```erlang
Pid = spawn(fun() -> ... end),
register(myproc, Pid).
unregister(myproc).



-record(city, {name, 
               infection_levels = [], 
               links = []}).
-record(city, {name :: atom(), 
               infection_levels = [] :: [{atom(), pos_integer()}], 
               links = [] :: [atom()]}).
               
City = #city{name = london, links = [essen, paris]},
Name = City#city.name.

#city{name = AName, links = Links} = City,
london = AName,
[essen, paris] = Links.

propagate_of(Disease, City = #city{links = Links}) -> 
  lists:foreach(fun(Linked) -> 
                  infect(Disease, Linked) 
                end, Links).
```

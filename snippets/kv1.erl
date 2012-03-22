-module(kv1).

-export([start/0, set/3, get/2]).

start() ->
    State = dict:new(),
    Handler = fun() -> loop(State) end,
    spawn(Handler).
    
set(Key, Value, Pid) ->
    Pid ! {set, Key, Value}.
    
get(Key, Pid) ->
    Pid ! {self(), {get, Key}},
    receive Value -> Value end.

loop(State) ->
    receive
        {From, {get, Key}} ->
            {ok, Value} = dict:find(Key, State),
            From ! Value,
            loop(State);
        {set, Key, Value} ->
            NewState = dict:store(Key, Value, State),
            loop(NewState)
    end.


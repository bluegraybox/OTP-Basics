-module(kv2).

-export([start/0, set/3, fetch/2, increment/2]).

start() ->
    State = dict:new(),
    Handler = fun() -> loop(State) end,
    spawn(Handler).
    
set(Key, Value, Pid) ->
    Pid ! {set, Key, Value}.
    
fetch(Key, Pid) ->
    Pid ! {self(), {fetch, Key}},
    receive Value -> Value end.

increment(Key, Pid) ->
    Pid ! {self(), {increment, Key}},
    receive Value -> Value end.

loop(State) ->
    receive
        {From, Message} ->
            {NewState, Value} = handle_call(Message, State),
            From ! Value;
        Message ->
            NewState = handle_cast(Message, State)
    end,
    loop(NewState).

handle_call({fetch, Key}, State) ->
    {ok, Value} = dict:find(Key, State),
    {State, Value};
handle_call({increment, Key}, State) ->
    NewState = dict:update_counter(Key, 1, State),
    {ok, Value} = dict:find(Key, NewState),
    {NewState, Value}.

handle_cast({set, Key, Value}, State) ->
    dict:store(Key, Value, State).


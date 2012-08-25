-module(kv3).

-export([start/0, set/3, fetch/2, increment/2]).

start() ->
    State = init(),
    Handler = fun() -> loop(State) end,
    spawn(Handler).

init() -> dict:new().

set(Key, Value, Pid) ->
    cast({set, Key, Value}, Pid).
    
fetch(Key, Pid) ->
    call({fetch, Key}, Pid).

increment(Key, Pid) ->
    call({increment, Key}, Pid).

cast(Message, Pid) ->
    Pid ! Message.

call(Message, Pid) ->
    Pid ! {self(), Message},
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


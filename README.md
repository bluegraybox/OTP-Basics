# OTP Basics

I'm going to try to sketch out here, as briefly as possible, what you need to know to wrap your head around Erlang's OTP framework.
I won't try to fill in all the details, but I'll build up the structure you can hang them on.

The place to start is with a very simple service: a key-value store.
You can set values, you can get values, and that's about it.
We'll use it something like this:

    S = kvstore:start().
    kvstore:set(name, "Colin", S).
    V = kvstore:get(name, S).

Behind the scenes, we're spinning off an Erlang process and sending messages to it.
We create an empty dictionary for its data, wrap it in a closure, and spawn it as a new process.

    start() ->
        State = dict:new(),
        Handler = fun() -> loop(State) end,
        spawn(Handler).
        
    set(Key, Value, Pid) ->
        Pid ! {set, Key, Value}.
        
    get(Key, Pid) ->
        Pid ! {self(), {get, Key}},
        receive Value -> Value end.

The `loop` function receives these requests, either updates its data or sends back a value, and tail-recurses.

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

That's it for a basic, functioning service. Now let's mess with it a bit.
If we look at the message passing, we see that `set` is a one-way request, and `get` is a two-way request.
Let's split that logic out a bit.

    loop(State) ->
        receive
            {From, Message} ->
                {NewState, Value} = handle_call(Message, State),
                From ! Value;
            Message ->
                NewState = handle_cast(Message, State),
        end,
        loop(NewState).
    
    handle_call({get, Key}, State) ->
        {ok, Value} = dict:find(Key, State),
        {State, Value}.
    
    handle_cast({set, Key, Value}, State) ->
        dict:store(Key, Value, State).

The important things here are that `loop` no longer has to know anything about the message content, and `handle_call` and `handle_cast` don't know anything about message passing.
For `loop`, the message is a black box.
`handle_call` and `handle_cast` are straight functions, so we can test them independently of the message passing.

As an aside, note that `handle_call` returns a new state, even though it doesn't change in this case.
That comes in handy if we want to add an `increment` function which returns the updated value.

    increment(Key, Pid) ->
        Pid ! {self(), {increment, Key}},
        receive Value -> Value end.

Then we can just add another `handle_call` clause, with no change to `loop`.

    handle_call({increment, Key}, State) ->
        NewState = dict:update_counter(Key, 1, State),
        {ok, Value} = dict:find(Key, NewState),
        {NewState, Value};

And that interaction looks something like:

    S = kvstore:start().
    kvstore:set(age, 45, S).
    V = kvstore:increment(age, S).

Now let's pull a similar job on the client functions. We'll split the message handling code into `call` and `cast`, logically enough.

    cast(Message, Pid) ->
        Pid ! Message.
    
    call(Message, Pid) ->
        Pid ! {self(), Message},
        receive Value -> Value end.

So now the client functions look like:

    set(Key, Value, Pid) ->
        cast({set, Key, Value}, Pid).
    
    get(Key, Pid) ->
        call({get, Key}, Pid).
    
    increment(Key, Pid) ->
        call({increment, Key}, Pid).

Not a huge improvement, but a bit tidier.
And now we give `start` a similar treatment, separating out the application-specific data initialization, and leaving just the functionality for spawning the now-generic message handling loop.

    start() ->
        State = init(),
        Handler = fun() -> loop(State) end,
        spawn(Handler).

    init() -> dict:new().



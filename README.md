# OTP Basics

I'm going to try to sketch out here, as briefly as possible, what you need to know to wrap your head around Erlang's OTP framework.
I won't try to fill in all the details, but I'll build up the structure you can hang them on.

## The Simplest Thing That Works

The place to start is with a very simple service: a key-value store.
You can set values, you can get values, and that's about it.
We'll use it something like this:

```erlang
S = kvstore:start().
kvstore:set(name, "Colin", S).
V = kvstore:get(name, S).
```

Behind the scenes, we're spinning off an Erlang process and sending messages to it.
We create an empty dictionary for its data, wrap it in a closure, and spawn it as a new process.

```erlang
start() ->
    State = dict:new(),
    Handler = fun() -> loop(State) end,
    spawn(Handler).
    
set(Key, Value, Pid) ->
    Pid ! {set, Key, Value}.
    
get(Key, Pid) ->
    Pid ! {self(), {get, Key}},
    receive Value -> Value end.
```

The `loop` function receives these requests, either updates its data or sends back a value, and tail-recurses.

```erlang
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
```

## Re-plumbing

That's it for a basic, functioning service. Now let's mess with it a bit.
If we look at the message passing, we see that `set` is a one-way request, and `get` is a two-way request.
Let's split that logic out a bit.

```erlang
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
```

The important things here are that `loop` no longer has to know anything about the message content, and `handle_call` and `handle_cast` don't know anything about message passing.
For `loop`, the message is a black box.
`handle_call` and `handle_cast` are straight functions, so we can test them independently of the message passing.

As an aside, note that `handle_call` returns a new state, even though it doesn't change in this case.
That comes in handy if we want to add an `increment` function which returns the updated value.

```erlang
increment(Key, Pid) ->
    Pid ! {self(), {increment, Key}},
    receive Value -> Value end.
```

Then we can just add another `handle_call` clause, with no change to `loop`.

```erlang
handle_call({increment, Key}, State) ->
    NewState = dict:update_counter(Key, 1, State),
    {ok, Value} = dict:find(Key, NewState),
    {NewState, Value};
```

And that interaction looks something like:

```erlang
S = kvstore:start().
kvstore:set(age, 45, S).
V = kvstore:increment(age, S).
```

Now let's pull the same job on the client functions. We'll split the message handling code into `call` and `cast`, logically enough.

```erlang
cast(Message, Pid) ->
    Pid ! Message.

call(Message, Pid) ->
    Pid ! {self(), Message},
    receive Value -> Value end.
```

So now the client functions look like:

```erlang
set(Key, Value, Pid) ->
    cast({set, Key, Value}, Pid).

get(Key, Pid) ->
    call({get, Key}, Pid).

increment(Key, Pid) ->
    call({increment, Key}, Pid).
```

Not a huge improvement, but a bit tidier.
And now we give `start` a similar treatment:
Initializing the state dictionary is the only part that's application-specific, so we'll split that out into a separate function.
That leaves just the functionality for spawning the now-generic message handling loop.

```erlang
start() ->
    State = init(),
    Handler = fun() -> loop(State) end,
    spawn(Handler).

init() -> dict:new().
```

## Taking Stock

So now we have a set of generic functions that deal with process spawning and message passing:

* `start/0`
* `cast/2`
* `call/2`
* `loop/1`

A set of business logic plugin functions:

* `init/0`
* `handle_cast`
* `handle_call`

And a set of custom API functions that hide the message format from the outside world:

* `set/3`
* `get/2`
* `increment/2`

They line up sorta like this:

<table>
  <tr><th> API         </th><th>             Generic </th><th>             Plugin        </th></tr>
  <tr><td>             </td><td>             start/0 </td><td>             init/1        </td></tr>
  <tr><td> set/3       </td><td>             cast/2  </td><td>             handle_cast/2 </td></tr>
  <tr><td> get/2       </td><td rowspan="2"> call/2  </td><td rowspan="2"> handle_call/2 </td></tr>
  <tr><td> increment/2 </td></tr>
  <tr><td>             </td><td>             loop/1  </td><td>                           </td></tr>
</table>

So, it looks like we've got some generic functions that we could take out and put in some kind of reusable framework.
Oh, no need! **It's already been done**.
This is pretty much what the OTP `gen_server` provides.
It's got a couple extra management functions, and a few other variations, but this is the gist of it.
If you understand what we've done so far, you can understand `gen_server`.



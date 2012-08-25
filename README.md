# OTP Basics - gen_server

This is the first part of an attempt to sketch out Erlang's OTP framework.
I'm not going to try to explain it in detail.
I'm just going to look at it from 30,000 feet and talk about the concepts you need to wrap your head around in order to understand it.
(This is of course as much about me making sure I really understand it.)

The idea is that you'll read this and think, "Oh, ok, I get what they're trying to do here and why it's useful,"
and then you'll go off and dig into [Erlang and OTP In Action](http://www.amazon.com/Erlang-OTP-Action-Martin-Logan/dp/1933988789/) to get all the details.
Think of this more as an orientation than a tutorial,
like taking a few moments with a crudely drawn map and a compass before you unlimber your machete and charge off into the woods.

The name `gen_server` may be a bit misleading.
While it *is* a framework for writing servers, these are servers in the Erlang sense.
They're managed processes that handle requests to access to a resource,
but that resource may well be just a data structure or a socket or something.
They're mostly very simple and small, fulfilling the role that objects do in OO languages.

Rather than having shared data structures and relying on threads to synchronize their access to them,
each Erlang data structure is local to a single process, and other processes send it messages to fetch or update its values.
Only letting one process access the data directly dodges the whole issue of concurrent access.
That sounds kinda crazy if you're new to it, but Erlang makes it really easy.
Let me show you how...

## The Simplest Thing That Works

The place to start is with a very simple service: a key-value store.
You can set values, you can fetch values, and that's about it.
We'll use it something like this:

```erlang
S = kvstore:start().
kvstore:set(name, "Colin", S).
V = kvstore:fetch(name, S).
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
    
fetch(Key, Pid) ->
    Pid ! {self(), {fetch, Key}},
    receive Value -> Value end.
```

The `loop` function receives these requests, either updates its data or sends back a value, and tail-recurses.

```erlang
loop(State) ->
    receive
        {From, {fetch, Key}} ->
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
If we look at the message passing, we see that `set` is a one-way request, and `fetch` is a two-way request.
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

handle_call({fetch, Key}, State) ->
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

fetch(Key, Pid) ->
    call({fetch, Key}, Pid).

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

A set of plugin functions to handle the business logic:

* `init/0`
* `handle_cast`
* `handle_call`

And a set of custom API functions that wrap this up and hide the message passing and formatting from the outside world:

* `set/3`
* `fetch/2`
* `increment/2`

They line up sorta like this:

<table>
  <tr><th> API         </th><th>             Generic </th><th>             Plugin        </th></tr>
  <tr><td>             </td><td>             start/0 </td><td>             init/1        </td></tr>
  <tr><td> set/3       </td><td>             cast/2  </td><td>             handle_cast/2 </td></tr>
  <tr><td> fetch/2       </td><td rowspan="2"> call/2  </td><td rowspan="2"> handle_call/2 </td></tr>
  <tr><td> increment/2 </td></tr>
  <tr><td>             </td><td>             loop/1  </td><td>                           </td></tr>
</table>

So, it looks like we've got some generic functions that we could take out and put in some kind of reusable framework.
And lo and behold, that's what `gen_server` has already done.

That's not all it's done, of course.
With process startup cleanly separated from business logic,
`gen_server` is able to manage the restart of servers after crashes,
and gives you tools for configuring that declaratively.

So don't worry, there's plenty more to learn,
but hopefully this gives you a bit more of a mental framework to hang all those pieces on.


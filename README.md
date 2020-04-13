# Flow

The point is that it is easy to define interactive processes at the
command-line:

``` haskell
foo :: IO ()
foo = do
  c <- getLine
  let i = case c of
        "a" -> 10
        "b" -> 20
        _ -> panic "First input must be 'a' or 'b'"
  a <- getNum
  b <- getNum
  print (i * (a + b))
```

This interaction all happens in a short span of time, and the haskell run-time
is kept alive between inputs. Now let's imagine that we wanted to code up the
same process over an API, with gaps between inputs of days, maybe even weeks or
months. This means that we can't keep the process alive, in this day and age of
cloud functions and Kubernetes we can't assume any server will be kept alive
very long, and regardless they should be "stateless". So that means we now need
to work out exactly what this process needs to "remember" each time it is
waiting for input:

- It needs to remember _where_ it is in the code, e.g. "I am currently blocked
  on the `getNum` of line 6".
- It needs to remember any state it has accumulated up to that point, e.g. the
  values of `i` and `a` in this case.

Both can be summarised as saying that the function needs to remember the
  _continuation_ function to be executed on the next input.

Another nice thing this does is that it know exactly which datatype it is
expecting as the next input, e.g. a number or a character, whereas an API either
has to rely on the user hitting the correct endpoint, or will have to accept
some generic sum-type.

As an API it would look something like this:

## Solutions

So you _have_ to persist _some_ state. Let's call that `s`.

The process must respond to inputs, let's call that `i`, we could have `i`
depend on `s`.

``` idris
respond : (x : s) -> i x -> s
```

Let's also assume that a workflow could have sub-workflows. Then a workflow
needs to be able to respond to a subworkflow terminating (or some other event,
let's just do termination for now).

``` haskell
respondInput :: s -> i -> s
respondSub :: t -> s -> s
```

that's too liberal, maybe in some cases it doesn't want to respond to user input.

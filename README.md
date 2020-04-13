# Optical machines

Allows you to create state-machines using programming patterns that look like
concurrency.

A `Flow i s m a` is a defines (the transition function) of a state-machine which
reacts to inputs of type `i`, has a state of type `s`, can perform effects in
the monad `m` and produces a value of type `a`.

The point is to define a larger flow in terms of smaller flows using this
function:

``` haskell
subflow ::
  (Monad m, Monoid r) =>
  -- | A focus on the state.
  (LensLike' (Focusing m r) a b) ->
  -- | A focus on the input.
  Getting (Endo [ib]) ia ib ->
  -- | The subflow to run on these foci.
  FlowT ib b m r ->
  FlowT ia a m r
```

and its indexed variant:

``` haskell
isubflow ::
  (Monad m, Eq i, Monoid r) =>
  -- | Indexed focus on the state.
  (IndexedTraversal' i a b) ->
  -- | A focus on the inputs which also extracts an index.
  Getting (Endo [(i, ib)]) ia (i, ib) ->
  -- | The subflow to run on these indexed foci.
  FlowT ib b m r ->
  FlowT ia a m r
```

These functions allow one to split up the logic of the state machine according
to optics on both the state and the input, thereby running sub-state-machines
sequentially, concurrently or a mix of the two.

## Small examples

All the following examples use the following basic machines:

``` haskell
f , g :: (Monad m) => FlowT Int Int m ()
f = (+) <$> ask >>= modify -- Increment the state by the input
g = (*) <$> ask >>= modify -- Multiply the state by the input
```

Execute a machine:

    λ> execFlow 10 2 f
    12
    λ> execFlow 10 2 g
    20

Run both `f` and `g` concurrently, on each input:

    λ> execFlow 10 (2,2) (subflow _1 identity f >> subflow _2 identity g)
    (12,20)

Run both `f` and `g` concurrently, splitting the input:

    λ> execFlow (20, 30) (2,2) (subflow _1 _1 f >> subflow _2 _2 g)
    (22,60)

Run both `f` and `g` concurrently, feeding `Left`s to `f` and `Right`s to `g`:

    λ> execFlow (Left 10) (2,2) (subflow _1 _Left f >> subflow _2 _Right g)
    (12,2)
    λ> execFlow (Right 10) (2,2) (subflow _1 _Left f >> subflow _2 _Right g)
    (2,20)

Run both `f` and `g` concurrently, sometimes getting inputs at the same time:

    λ> import Data.These
    λ> import Data.These.Lens
    λ> execFlow (This 10) (2,2) (subflow _1 here f >> subflow _2 there g)
    (12,2)
    λ> execFlow (That 10) (2,2) (subflow _1 here f >> subflow _2 there g)
    (2,20)
    λ> execFlow (These 10 20) (2,2) (subflow _1 here f >> subflow _2 there g)
    (12,40)

Run a map of `f` machines concurrently, distributing indexed inputs as
appropriate.

    λ> execFlow [('a', 1), ('b', 2), ('a', 3)] (fromList [('a', 0), ('b', 0)]) (isubflow itraversed traverse f)
    fromList [('a',4),('b',2)]

Run a list of `f` machines, feeding each input to all machines:

    λ> execFlow 10 [1,2,3,4] (subflow traverse identity f)
    [11,12,13,14]

Run a list of machines concurrently, running `f` and `g` sequentially on each
machine, for all inputs:

    λ> execFlow 10 [1,2,3,4] (subflow traverse identity f >> subflow traverse identity g)
    [110,120,130,140]

Run a list of machines concurrently, running `f` on the odd indices and `g` on
the even indices:

    λ> execFlow 10 [1,2,3,4,5,6] (subflow (itraversed . indices odd) identity f
    >> subflow (itraversed . indices even) identity g)
    [10,12,30,14,50,16]

Run a list of machines concurrently, feeding `This`s to `f` running of the odd
indices, and feeding `That`s to `g` running on the even indices, and feeding
`These` to both.

    λ> execFlow (This 10) [1,2,3,4,5,6] (subflow (itraversed . indices odd) here
    f >> subflow (itraversed . indices even) there g)
    [1,12,3,14,5,16]
    λ> execFlow (That 10) [1,2,3,4,5,6] (subflow (itraversed . indices odd) here f >> subflow (itraversed . indices even) there g)
    [10,2,30,4,50,6]
    λ> execFlow (These 10 20) [1,2,3,4,5,6] (subflow (itraversed . indices odd) here f >> subflow (itraversed . indices even) there g)
    [20,12,60,14,100,16]

## Extended example

Here we have an example of a game with 2 stages.
- In stage 1 multiple players are spawned which accumuate some points.
- The first to get more than 10 points wins and goes to stage 2.
- In stage 2 we continue accumulating some points.

See [the
example](https://github.com/jameshaydon/optical-machines/blob/master/src/Example.hs)
for the types.

``` haskell
game :: FlowT Input Game Identity ()
game = do
  -- Run stage 1:
  subflow _Stage1 _Stage1Input stage1
  -- Transition from stage 1 to stage 2:
  whenever (_Stage1 . players . itraversed . _Won . withIndex) $ \(i, x) ->
    put (Stage2 (MkStage2 i x))
  -- Run stage 2:
  subflow (_Stage2 . points) _Stage2Input $ stage2
  where
    stage1 = do
      -- Run all the player workflows:
      isubflow (players . itraversed) _Add player
      -- Create new player workflows:
      subflow identity _New $ do
        id <- use nextId
        nextId .= id + 1
        players . at id ?= Playing 0
    stage2 = do
      more <- (+) <$> ask
      modify more

player :: FlowT Int Player Identity ()
player = do
  currentState <- get
  case currentState of
    Playing x -> do
      i <- ask
      let x' = i + x
      put $
        if x' > 10
          then Won x'
          else Playing x'
    Won _ -> pure ()
```

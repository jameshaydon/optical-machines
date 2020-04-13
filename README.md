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

...

## Extended example

Here we have an example of a game with 2 stages.
- In stage 1 multiple players are spawned which accumuate some points.
- The first to get more than 10 points wins and goes to stage 2.
- In stage 2 we continue accumulating some points.

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

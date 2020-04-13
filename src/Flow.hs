{-# LANGUAGE GADTs #-}

module Flow where

import Control.Lens
import Control.Lens.Internal.Zoom
import Protolude

newtype FlowT i s m a = FlowT {getFlow :: ReaderT i (StateT s m) a}
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadReader i,
      MonadState s,
      MonadIO
    )

runPureFlow :: forall i s a. FlowT i s Identity a -> [i] -> s -> s
runPureFlow f is s = foldl' ff s is
  where
    ff s' i = runIdentity $ execFlow i s' f

runFlow :: i -> s -> FlowT i s m a -> m (a, s)
runFlow i s (FlowT f) = runStateT (runReaderT f i) s

execFlow :: (Monad m) => i -> s -> FlowT i s m a -> m s
execFlow i s (FlowT f) = execStateT (runReaderT f i) s

-- | Execute some focus of the input on some focus of the state.
--
-- @
-- subflow focusState focusInput f
-- @
--
-- runs the flow @f@ while focusing the state with @focusState@ and focusing the
-- inputs with @focusInput@.
--
-- The @focusState@ optic controls which part of the state the various subflows
-- operate on. The @focusInput@ optic controls how to flow input into the
-- various subflows. Combinations of various sorts of optics will correspond to
-- various notions of concurrent or sequential processing of the subflows.
--
-- Here are some examples, all of them assume:
-- @
-- f, g :: (Monad m) => FlowT Int Int m ()
-- f = modify =<< (+) <$> ask -- add the input to the state
-- g = modify =<< (*) <$> ask -- multiply the state by the input
-- @
--
-- @
-- subflow _1 id f >> subflow _2 id g
-- @
--
-- will run a flow with state @(a, b)@ that is composed of two concurrent flows,
-- with inputs being sent to /both/.
--
-- @
-- do subflow _1 _Left f
--    subflow _2 _Right g
-- @
--
-- will run 2 concurrent flows while sending `Left` inputs to the first and
-- `Right` inputs to the second.
--
-- When @focusState@ is a traversal the subflow is run concurrently on all the
-- targets. If each input is meant to run on /all/ of the subflows, this is what
-- you want, otherwise you probably want to use 'subflows' instead.
--
-- >>> execFlow 10 0 f
-- 10
--
-- >>> execFlow 10 0 (subflow identity identity f)
-- 10
--
-- >>> execFlow 10 (0,0) (subflow _1 identity f)
-- (10,0)
--
-- >>> execFlow (10,20) (0,0) (subflow _1 _1 f >> subflow _2 _2 f)
-- (10,20)
--
-- >>> execFlow (Left 10) (0,0) (subflow _1 _Left f >> subflow _2 _Right f)
-- (10,0)
subflow ::
  (Monad m, Monoid r) =>
  -- | A focus on the state.
  (LensLike' (Focusing m r) a b) ->
  -- | A focus on the input.
  Getting (Endo [ib]) ia ib ->
  -- | The subflow to run on this restriction.
  FlowT ib b m r ->
  FlowT ia a m r
subflow pState pIn (FlowT f) =
  FlowT $
    mconcat <$> (asks (^.. pIn) >>= traverse (zoom pState . lift . runReaderT f))

-- | Restrict indexed inputs to an indexed portion of the state.
subflows ::
  (Monad m, Eq i, Monoid r) =>
  -- | Multiple indexed portions of the state.
  (IndexedTraversal' i a b) ->
  -- | A focus on the input which also extracts an index.
  Getting (First (i, ib)) ia (i, ib) ->
  -- | The subflow to run on this restriction.
  FlowT ib b m r ->
  FlowT ia a m r
subflows subs subIn (FlowT f) = FlowT $ do
  ia <- ask
  case ia ^? subIn of
    Nothing -> pure mempty
    Just (i, ib) -> zoom (subs . index i) $ lift (runReaderT f ib)

-- | Restrict the state.
during :: (Monad m) => Prism' a a' -> FlowT ia a' m () -> FlowT ia a m ()
during p = subflow p identity

-- | Restrict the input.
listenFor :: (Monad m) => Prism' ia ia' -> FlowT ia' a m () -> FlowT ia a m ()
listenFor p = subflow identity p

whenever :: (Monad m, Monoid r) => Traversal' a b -> (b -> FlowT i a m r) -> FlowT i a m r
whenever tr f = do
  s <- get
  let xs = s ^.. tr
  rs <- traverse f xs
  pure (mconcat rs)

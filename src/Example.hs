module Example where

import Control.Lens
import Data.Generics.Product
import Data.Generics.Sum
-- import Data.These
-- import Data.These.Lens
import Flow
import Protolude

--

f , g :: (Monad m) => FlowT Int Int m ()
f = modify =<< (+) <$> ask
g = modify =<< (*) <$> ask

--

data Stage1
  = MkStage1
      { _nextId :: Int,
        _smalls :: Map Int Small
      }
  deriving stock (Show, Generic)

smalls :: Lens' Stage1 (Map Int Small)
smalls = field @"_smalls"

nextId :: Lens' Stage1 Int
nextId = field @"_nextId"

data Stage2
  = MkStage2
      { _winner :: Int,
        _points :: Int
      }
  deriving stock (Show, Generic)

points :: Lens' Stage2 Int
points = field @"_points"

data Big
  = Stage1 Stage1
  | Stage2 Stage2
  deriving stock (Show, Generic)

_Stage1 :: Prism' Big Stage1
_Stage1 = _Ctor @"Stage1"

_Stage2 :: Prism' Big Stage2
_Stage2 = _Ctor @"Stage2"

data Small
  = Small Int
  | SmallDone Int
  deriving stock (Show, Generic)

_SmallDone :: Prism' Small Int
_SmallDone = _Ctor @"SmallDone"

data In1
  = New
  | Add Int Int
  deriving stock (Generic)

_New :: Prism' In1 ()
_New = _Ctor @"New"

_Add :: Prism' In1 (Int, Int)
_Add = _Ctor @"Add"

data In = In1 In1 | In2 Int
  deriving stock (Generic)

_In1 :: Prism' In In1
_In1 = _Ctor @"In1"

_In2 :: Prism' In Int
_In2 = _Ctor @"In2"

big :: FlowT In Big Identity ()
big = do
  -- Run stage 1:
  subflow _Stage1 _In1 stage1
  -- Transition from stage 1 to stage 2:
  whenever (_Stage1 . smalls . itraversed . _SmallDone . withIndex) $ \(i, x) ->
    put (Stage2 (MkStage2 i x))
  -- Run stage 2:
  subflow (_Stage2 . points) _In2 $ do
    more <- (+) <$> ask
    modify more
  where
    --modify (+ more)

    stage1 = do
      -- Run all the small workflows:
      subflows (smalls . itraversed) _Add small
      -- Create new small workflows:
      subflow identity _New $ do
        id <- use nextId
        nextId .= id + 1
        smalls . at id ?= Small 0

small :: FlowT Int Small Identity ()
small = do
  s <- get
  case s of
    Small x -> do
      i <- ask
      let x' = i + x
      put $
        if x' > 10
          then SmallDone x'
          else Small x'
    SmallDone _ -> pure ()

example :: Big
example = runPureFlow big inputs (Stage1 (MkStage1 0 mempty))
  where
    inputs =
      [ new,
        new,
        add 0 3,
        add 1 2,
        add 0 3,
        add 1 4,
        add 0 5,
        In2 100
      ]
    add x y = In1 (Add x y)
    new = In1 New

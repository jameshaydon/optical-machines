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
        _players :: Map Int Player
      }
  deriving stock (Show, Generic)

players :: Lens' Stage1 (Map Int Player)
players = field @"_players"

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

data Game
  = Stage1 Stage1
  | Stage2 Stage2
  deriving stock (Show, Generic)

_Stage1 :: Prism' Game Stage1
_Stage1 = _Ctor @"Stage1"

_Stage2 :: Prism' Game Stage2
_Stage2 = _Ctor @"Stage2"

data Player
  = Playing Int
  | Won Int
  deriving stock (Show, Generic)

_Won :: Prism' Player Int
_Won = _Ctor @"Won"

data Stage1Input
  = New
  | Add Int Int
  deriving stock (Generic)

_New :: Prism' Stage1Input ()
_New = _Ctor @"New"

_Add :: Prism' Stage1Input (Int, Int)
_Add = _Ctor @"Add"

data Input
  = Stage1Input Stage1Input
  | Stage2Input Int
  deriving stock (Generic)

_Stage1Input :: Prism' Input Stage1Input
_Stage1Input = _Ctor @"Stage1Input"

_Stage2Input :: Prism' Input Int
_Stage2Input = _Ctor @"Stage2Input"

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

endState :: Game
endState = runPureFlow game inputs (Stage1 (MkStage1 0 mempty))
  where
    inputs =
      [ new,
        new,
        add 0 3,
        add 1 2,
        add 0 3,
        add 1 4,
        add 0 5,
        Stage2Input 100
      ]
    add x y = Stage1Input (Add x y)
    new = Stage1Input New

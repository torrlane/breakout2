{-# LANGUAGE TemplateHaskell #-}
module Engine where

import Control.Lens

data BreakoutGame = Game
  { 
    _ball :: Ball
  , _paddle :: Paddle
  , _actions :: [Action] -- ^ actions that have occurred, but not yet been processed
  , _wallWidth :: Float -- ^ the thickness of the game walls
  } deriving Show

data Action = MoveLeft | MoveRight deriving Show

type Seconds = Float
type Position = (Float, Float)
type Velocity = (Float, Float)
type Radius = Float

data Wall = Wall { _wHeight :: Float, _wWidth :: Float, _wPosition :: Position} deriving Show
data Ball = Ball { _bPosition :: Position, _bRadius :: Radius, _bVelocity :: Velocity} deriving Show
data Paddle = Paddle { _pPosition :: Position, _pWidth :: Float, _pHeight :: Float } deriving Show

data BlockState = Empty | Block
type BlockGrid = [[BlockState]]

data PlayerStatistics = PlayerStatistics {score :: Int, level :: Int }
data GameDisplay = GameDisplay {blockwidth :: Float, blockHeight :: Float}


makeLenses ''BreakoutGame

makeLenses ''Paddle

width, height :: Float
width = 300
height = 300
-- | Update the ball position using its current velocity.
moveBall :: Seconds    -- ^ The number of seconds since last update
         -> BreakoutGame -- ^ The initial game state
         -> BreakoutGame -- ^ A new game state with an updated ball position
moveBall seconds game = game { _ball = b' }
  where
    b = _ball game
    -- Old locations and velocities.
    (x, y) = _bPosition b
    (vx, vy) = _bVelocity b
    -- New locations.
    x' = x + vx * seconds
    y' = y + vy * seconds
    b' = b { _bPosition = (x', y') }

performActions :: BreakoutGame -> BreakoutGame
performActions game = game' { _actions = [] }
  where
    as = _actions game
    game' = foldr movePaddle game as

-- Lens tutorial https://mmhaskell.com/blog/2017/6/12/taking-a-close-look-at-lenses
movePaddle :: Action -> BreakoutGame -> BreakoutGame
  -- set the x coordinate of position to the value (pxNew a x)
movePaddle a game = game & (paddle . pPosition . _1) %~ (pxNew a) 
  where
    pw = view (paddle . pWidth) game 
    maxWidth = (width / 2) - (pw / 2) - (_wallWidth game)
    pxNew MoveLeft px = max (px - 10) (-maxWidth)
    pxNew MoveRight px = min (px + 10) maxWidth

-- | Given position and radius of the ball, return whether a collision occurred.
vertWallCollision :: Position -> Float -> Radius -> Bool 
vertWallCollision (_, y) w radius = topCollision || bottomCollision
  where
    topCollision    = y + radius >= (height / 2) - w
    bottomCollision = y - radius <= -(height / 2) + w

horizWallCollision :: Position -> Float -> Radius -> Bool 
horizWallCollision (x, _) w radius = leftCollision || rightCollision
  where
    leftCollision    = x + radius >= (width / 2) - w
    rightCollision = x - radius <= -(width / 2) + w

wallBounce :: BreakoutGame -> BreakoutGame
wallBounce game = game { _ball = b'  }
  where
    b = _ball game
    r = _bRadius b
    w = _wallWidth game
    (vx, vy) = _bVelocity b
    vy' = if vertWallCollision (_bPosition b) w r then -vy else vy
    vx' = if horizWallCollision (_bPosition b) w r then -vx else vx
    b' = b { _bVelocity= (vx', vy') }


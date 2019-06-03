{-# LANGUAGE TemplateHaskell #-}
module Engine where

import Control.Lens

data BreakoutGame = Game
  { 
    _ball :: Ball
  , _prevBall :: Ball
  , _paddle :: Paddle
  , _actions :: [Action] -- ^ actions that have occurred, but not yet been processed
  , _wallWidth :: Float -- ^ the thickness of the game walls
  } deriving Show

data Action = MoveLeft | MoveRight deriving Show

type Seconds = Float
type Position = (Float, Float)
type Velocity = (Float, Float)
type Radius = Float

data Ball = Ball { _bPosition :: Position, _bRadius :: Radius, _bVelocity :: Velocity, _tangent :: Float } deriving Show
data Paddle = Paddle { _pPosition :: Position, _pWidth :: Float, _pHeight :: Float } deriving Show
data Wall = Wall { _wPosition :: Position, _wWidth :: Float, _wHeight :: Float } deriving Show

data Direction = N | NE | E | SE | S | SW | W | NW

bDirection :: Ball -> Direction
bDirection ball
  | vx < 0 && vy < 0  = SW
  | vx < 0 && vy == 0 = W
  | vx < 0 && vy > 0  = NW
  | vx == 0 && vy < 0 = S
  | vx == 0 && vy > 0 = N
  | vx > 0 && vy < 0  = SE
  | vx > 0 && vy == 0 = E
  | vx > 0 && vy > 0 = NE
  where
    (vx, vy) = _bVelocity ball
    


  -- equation = y = (vx/vy) x + (by - vx/vy * bx)

data BlockState = Empty | Block
type BlockGrid = [[BlockState]]

data PlayerStatistics = PlayerStatistics {score :: Int, level :: Int }
data GameDisplay = GameDisplay {blockwidth :: Float, blockHeight :: Float}


makeLenses ''BreakoutGame

makeLenses ''Paddle

width, height :: Float
width = 300
height = 150
-- | Update the ball position using its current velocity.
moveBall :: Seconds    -- ^ The number of seconds since last update
         -> BreakoutGame -- ^ The initial game state
         -> BreakoutGame -- ^ A new game state with an updated ball position
moveBall seconds game = game { _prevBall = b, _ball = b' }
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
    pxNew MoveLeft px = max (px - pIncrement) (-maxWidth)
    pxNew MoveRight px = min (px + pIncrement) maxWidth
    pIncrement = 10



-- | Given position, wallWidth and radius of the ball, return whether a collision occurred.
vertWallCollision :: Ball -> Float -> Bool 
vertWallCollision ball w = topCollision || bottomCollision
  where
    topCollision    = y >= (height / 2) - w
    bottomCollision = False -- y <= -(height / 2) + w
    (_, y) = _bPosition ball

horizWallCollision :: Ball -> Float -> Bool 
horizWallCollision ball w = leftCollision || rightCollision
  where
    leftCollision  = x >= (width / 2) - w
    rightCollision = x <= -(width / 2) + w
    (x, _) = _bPosition ball

wallBounce :: BreakoutGame -> BreakoutGame
wallBounce game = game { _ball = b'  }
  where
    b = _ball game
    w = _wallWidth game
    (vx, vy) = _bVelocity b
    vy' = if vertWallCollision b w then -vy else vy
    vx' = if horizWallCollision b w  then -vx else vx
    b' = b { _bVelocity= (vx', vy'), _tangent = (vy' / vx') }

isBallInPaddle :: Ball -> Paddle -> Bool
isBallInPaddle ball paddle = paddleYAlign && paddleXAlign
  where
    (bx, by) = _bPosition ball
    (px, py) = _pPosition paddle
    h = (_pHeight paddle) / 2
    w = (_pWidth paddle) / 2
    paddleYAlign = (py - h) < by && (py + h) > by
    paddleXAlign = (px - w) < bx && (px + w) > bx
    

paddleBounce :: BreakoutGame -> BreakoutGame
paddleBounce game =
  game { _ball = b' }
  where
    b = _ball game
    p = _paddle game
    (vx, vy) = _bVelocity b
    vy' = if paddleCollision && isTopCollision && vy < 0 then -vy else vy
    vx' = if paddleCollision && isSideCollision then -vx else vx
    b' = b { _bVelocity=(vx', vy'), _tangent = (vy' / vx')}
    mp = paddleTopCollisionPoint p b 
    isSideCollision = (mp == Nothing)
    isTopCollision = not isSideCollision
    paddleCollision = isBallInPaddle b p

paddleTopCollisionPoint :: Paddle -> Ball -> Maybe Position
paddleTopCollisionPoint paddle ball =
  if (px - w < cx && px + w > cx) then Just (cx, py + h) else Nothing
  where
    (vx, vy) = _bVelocity ball
    (bx, by) = _bPosition ball
    (px, py) = _pPosition paddle
    h = (_pHeight paddle) / 2
    w = (_pWidth paddle) / 2
    m = _tangent ball
    c = by - (m * bx)
    cx = (py + h - c) / m
    

-- m = vy/vx
-- y = mx + c
-- by = m*bx + c
-- c = by - m*bx

-- py + h = mx + c
-- tf (py +h -c)/m = x ( test whether x is within px-w, px+w )
-- 

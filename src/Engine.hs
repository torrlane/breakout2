module Engine where

data BreakoutGame = Game
  { 
    _ball :: Ball
  , _paddle :: Paddle
  , _walls :: [Wall] -- ^ walls surrounding the playing area
  , _actions :: [Action] -- ^ actions that have occurred, but not yet been processed
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
performActions game = game { _paddle = paddle', _actions = [] }
  where
    as = _actions game
    p = _paddle game
    paddle' = foldr movePaddle p as

movePaddle :: Action -> Paddle -> Paddle
movePaddle a p = p { _pPosition = (pxNew a, py)}
  where
    pp = _pPosition p
    px = fst pp
    py = snd pp
    pw = _pWidth p
    w = (width / 2) - (pw / 2)
    pxNew MoveLeft = max (px - 10) (-w)
    pxNew MoveRight = min (px + 10) w

-- | Given position and radius of the ball, return whether a collision occurred.
wallCollision :: Position -> Radius -> Bool 
wallCollision (_, y) radius = topCollision || bottomCollision
  where
    topCollision    = y - radius <= (width / 2)
    bottomCollision = y + radius >= (width / 2)

wallBounce :: BreakoutGame -> BreakoutGame
wallBounce game = game { _ball = b'  }
  where
    b = _ball game
    r = _bRadius b
    -- The old velocities.
    (vx, vy) = _bVelocity b
    vy' = if wallCollision (_bPosition b) r
          then
             -- Update the velocity.
             -vy
           else
            -- Do nothing. Return the old velocity.
            vy
    b' = b { _bVelocity= (vx, vy') }


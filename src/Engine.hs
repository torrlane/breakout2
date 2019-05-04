module Engine where

-- | Data describing the state of the pong game.
data BreakoutGame = Game
  { 
    ball :: Ball
  , paddle :: Paddle           -- ^ player paddle
  , walls :: [Wall] -- ^ walls surrounding the playing area
  , actions :: [Action] -- ^ actions that have occurred, but not yet been processed
  } deriving Show

data Action = MoveLeft | MoveRight deriving Show

type Seconds = Float
type Position = (Float, Float)
type Velocity = (Float, Float)
type Radius = Float

data Wall = Wall { wHeight :: Float, wWidth :: Float, wPosition :: Position} deriving Show
data Ball = Ball { bPosition :: Position, bRadius :: Radius, bVelocity :: Velocity} deriving Show
data Paddle = Paddle { pPosition :: Position, pWidth :: Float, pHeight :: Float, pVelocity :: Velocity } deriving Show

data BlockState = Empty | Block
type BlockGrid = [[BlockState]]


-- data Game = Game { ball :: Ball, paddle :: Paddle, grid :: BlockGrid }
data PlayerStatistics = PlayerStatistics {score :: Int, level :: Int }

data GameDisplay = GameDisplay {blockwidth :: Float, blockHeight :: Float}


-- globals:
-- blockWidth, blockHeight, ballRadius, paddleWidth, paddleHeight


width, height :: Float
width = 300
height = 300
-- | Update the ball position using its current velocity.
moveBall :: Seconds    -- ^ The number of seconds since last update
         -> BreakoutGame -- ^ The initial game state
         -> BreakoutGame -- ^ A new game state with an updated ball position
moveBall seconds game = game { ball = b' }
  where
    b = ball game
    -- Old locations and velocities.
    (x, y) = bPosition b
    (vx, vy) = bVelocity b
    -- New locations.
    x' = x + vx * seconds
    y' = y + vy * seconds
    b' = b { bPosition = (x', y') }

performActions :: BreakoutGame -> BreakoutGame
performActions game = game { paddle = paddle', actions = [] }
  where
    as = actions game
    p = paddle game
    paddle' = foldr movePaddle p as

movePaddle :: Action -> Paddle -> Paddle
movePaddle a p = p { pPosition = (pxNew a, py)}
  where
    pp = pPosition p
    px = fst pp
    py = snd pp
    pw = pWidth p
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
wallBounce game = game { ball = b'  }
  where
    b = ball game
    r = bRadius b
    -- The old velocities.
    (vx, vy) = bVelocity b
    vy' = if wallCollision (bPosition b) r
          then
             -- Update the velocity.
             -vy
           else
            -- Do nothing. Return the old velocity.
            vy
    b' = b { bVelocity= (vx, vy') }


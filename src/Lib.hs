module Lib
    ( start
    ) where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game
import Engine
import Render

start :: IO ()
start = play window background fps initialState render handleKeys update
  where
    frame :: Seconds -> Picture
    frame seconds = render $ moveBall seconds initialState

-- | Update the game by moving the ball.
-- Ignore the ViewPort argument.
update ::  Seconds -> BreakoutGame -> BreakoutGame
update seconds = performActions . wallBounce . moveBall seconds

window :: Display
window = InWindow "window" (round width, round height) windowPosition

windowPosition = (10, 10)

-- | Number of frames to show per second.
fps :: Int
fps = 60


-- | The starting state for the game of Pong.
initialState :: BreakoutGame
initialState = Game
  { ball = Ball { bPosition = (-10, 30), bRadius = 10, bVelocity = (30, -33)}
  , paddle = Paddle { pPosition = (40, - (height / 2) + 26), pWidth = 86, pHeight = 26, pVelocity = (0, 0 )}
  , walls = [ Wall { wHeight = 10, wWidth = width, wPosition = (0, (height / 2) - 5) },
              Wall { wHeight = height, wWidth = 10, wPosition = ((width / 2) - 5, 0) },
              Wall { wHeight = height, wWidth = 10, wPosition = (-(width / 2) + 5, 0) }
            ]
  , actions = []
  }

handleKeys :: Event -> BreakoutGame -> BreakoutGame
-- 'h' = movePaddle left
handleKeys (EventKey (Char 'h') _ _ _) game = game { actions = (MoveLeft : as) }
  where
    as = actions game
-- 'l' = movePaddle right
handleKeys (EventKey (Char 'l') _ _ _) game = game { actions = (MoveRight : as) }
  where
    as = actions game
-- For an 's' keypress, reset the ball to the center.
handleKeys (EventKey (Char 's') _ _ _) game = game { ball = b' }
  where
    b = ball game
    b' = b {bPosition = (0, 0)}
-- Do nothing for all other events.
handleKeys _ game = game


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
update seconds = performActions . paddleBounce . wallBounce . moveBall seconds

window :: Display
window = InWindow "window" (round width, round height) windowPosition

windowPosition = (10, 10)

-- | Number of frames to show per second.
fps :: Int
fps = 30


-- | The starting state for the game of Pong.
initialState :: BreakoutGame
initialState = Game
  { _ball = Ball { _bPosition = (60, 30), _bRadius = 5, _bVelocity = (-60, -60), _tangent = 1 }
  , _prevBall = Ball { _bPosition = (-10, 30), _bRadius = 1, _bVelocity = (-60, -60), _tangent = 1 }
  , _paddle = Paddle { _pPosition = (40, - (height / 2) + 13), _pWidth = 86, _pHeight = 26}
  , _actions = []
  , _wallWidth = 10
  }

handleKeys :: Event -> BreakoutGame -> BreakoutGame
-- 'h' = movePaddle left
handleKeys (EventKey (Char 'h') _ _ _) game = game { _actions = (MoveLeft : as) }
  where
    as = _actions game
-- 'l' = movePaddle right
handleKeys (EventKey (Char 'l') _ _ _) game = game { _actions = (MoveRight : as) }
  where
    as = _actions game
-- For an 's' keypress, reset the ball to the center.
handleKeys (EventKey (Char 's') _ _ _) game = initialState
-- Do nothing for all other events.
handleKeys _ game = game


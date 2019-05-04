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
  { _ball = Ball { _bPosition = (-10, 30), _bRadius = 10, _bVelocity = (30, -33)}
  , _paddle = Paddle { _pPosition = (40, - (height / 2) + 26), _pWidth = 86, _pHeight = 26}
  , _walls = [ Wall { _wHeight = 10, _wWidth = width, _wPosition = (0, (height / 2) - 5) },
              Wall { _wHeight = height, _wWidth = 10, _wPosition = ((width / 2) - 5, 0) },
              Wall { _wHeight = height, _wWidth = 10, _wPosition = (-(width / 2) + 5, 0) }
            ]
  , _actions = []
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
handleKeys (EventKey (Char 's') _ _ _) game = game { _ball = b' }
  where
    b = _ball game
    b' = b {_bPosition = (0, 0)}
-- Do nothing for all other events.
handleKeys _ game = game


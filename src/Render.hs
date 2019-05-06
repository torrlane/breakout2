module Render
    ( render, background
    ) where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game
import Engine


-- | Convert a game state into a picture.
render :: BreakoutGame  -- ^ The game state to render.
       -> Picture   -- ^ A picture of this game state.
render game =
  pictures [renderBall game, renderWalls game, renderPaddle game]

renderBall :: BreakoutGame -> Picture
renderBall game = translate x y $ color ballColor $ circleSolid r
  where
    b = _ball game
    r = _bRadius b
    (x, y) = _bPosition b

renderPaddle :: BreakoutGame -> Picture
renderPaddle game = pictures
  [ translate x y $ color paddleOuterColor $ rectangleSolid w h
  , translate x y $ color paddleInnerColor $ rectangleSolid (w - 6) (h - 6)
  ]
  where
    p = _paddle game
    (x, y) = _pPosition p
    w = _pWidth p
    h = _pHeight p

  
paddleInnerColor = light (light blue)
paddleOuterColor = rose

renderWalls :: BreakoutGame -> Picture
renderWalls game = pictures $ map renderWall $ _walls
  where
    wallWidth = _wallWidth game
    horizStart = (width - wallWidth) / 2
    vertStart = (height - wallWidth) / 2
    topWall = Wall { _wHeight = wallWidth, _wWidth = width, _wPosition = (0, vertStart) }
    rightWall = Wall { _wHeight = height, _wWidth = wallWidth, _wPosition = (horizStart, 0) }
    leftWall = Wall { _wHeight = height, _wWidth = wallWidth, _wPosition = (-horizStart, 0) }
    _walls = [ topWall, rightWall, leftWall ]

renderWall :: Wall -> Picture
renderWall wall = translate x y $
        color wallColor $
          rectangleSolid w h
  where
    (x, y) = _wPosition wall
    w = _wWidth wall
    h = _wHeight wall


wallColor = greyN 0.5

background :: Color
background = white

ballColor = dark red

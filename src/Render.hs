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
renderBall game = uncurry translate p $ color ballColor $ circleSolid r
  where
    b = _ball game
    p = _bPosition b
    r = _bRadius b

renderPaddle :: BreakoutGame -> Picture
renderPaddle game = pictures
  [ translate x y $ color paddleOuterColor $ rectangleSolid w h
  , translate x y $ color paddleInnerColor $ rectangleSolid (w - 6) (h - 6)
  ]
  where
    p = _paddle game
    x = fst $ _pPosition p
    y = snd $ _pPosition p
    w = _pWidth p
    h = _pHeight p

  
paddleInnerColor = light (light blue)
paddleOuterColor = rose

renderWalls :: BreakoutGame -> Picture
renderWalls game = pictures $ map renderWall $ _walls game

renderWall :: Wall -> Picture
renderWall wall = translate x y $
        color wallColor $
          rectangleSolid w h
  where
    x = fst (_wPosition wall)
    y = snd (_wPosition wall)
    w = _wWidth wall
    h = _wHeight wall


wallColor = greyN 0.5

background :: Color
background = white

ballColor = dark red

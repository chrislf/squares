module Circs where

import Graphics.Gloss
import qualified Data.Text as T

libMain :: IO ()
libMain =
  animate
    (InWindow "Squares" (1000, 1000) (100, 100))
    black
    $ manyCircles 20 10

type Count = Float
type Radius = Float
type Time = Float

xyToAlpha x y xmax ymax t = makeColor 1 1 1 ((1 + sin (t+x))/2)

manyCircles :: Count -> Radius -> Time -> Picture
manyCircles i r t = Pictures (time: sint : pics)
  where
    --colouredCirc x y = Color (xyToColour (x+t*10) (y+t*10) (i*2) (i*2)) $ circleSolid r
    colouredCirc x y = Color (xyToAlpha x y (i*2) (i*2) t) $ circleSolid r
    pics = [ Translate (x*r*2) (y*r*2) (colouredCirc x y) | x <- [-i..i], y <- [-i..i] ]
    time = Translate (-i*r*2) (i*r*2.1) . Scale 0.25 0.25 . Color white . Text . show $ t
    sint = Translate (-i*r*2) (i*r*2.2) . Scale 0.25 0.25 . Color white . Text . show $ sin t

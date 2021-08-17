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

xyToColour :: Float -> Float -> Float -> Float -> Color
xyToColour x y xmax ymax = makeColor (x/xmax) (y/ymax) (x+y/xmax+ymax) 1

manyCircles :: Count -> Radius -> Time -> Picture
manyCircles i r t = Pictures (time : pics)
  where
    colouredCirc x y = Color (xyToColour (x+t) (y+t) (i*2) (i*2)) $ circleSolid r
    pics = [ Translate (x*r*2) (y*r*2) (colouredCirc x y) | x <- [-i..i], y <- [-i..i] ]
    time = Translate (-i*r*2) (i*r*2.1) . Scale 0.25 0.25 . Color white . Text . show $ t

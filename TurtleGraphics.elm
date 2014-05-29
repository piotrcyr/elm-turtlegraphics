module TurtleGraphics where

import Graphics.Collage(..)
import Mouse(..)

infixl 1 >>
infixl 1 >.

type Turtle = (Float, [[(Float, Float)]])
type Trace = Turtle -> Turtle

turtleAt : (Float, Float) -> Turtle
turtleAt = \pos -> (0, [[pos]])

join : Trace -> Trace -> Trace
join = flip (.)

(>>) : Trace -> Trace -> Trace
(>>) = join

grow : Trace -> Trace -> Trace
grow  tr1 tr2 = \turtle -> let (a, ((x,y)::ps)::pss) = tr1 turtle
                               (_, pss') = tr2 (a, ((x,y)::ps)::pss)
                           in (a, [(x,y)]::pss')

(>.) : Trace -> Trace -> Trace
(>.) = grow

connect : [Trace] -> Trace
connect = \traces -> \turtle -> foldl (<|) turtle traces

sprout : [Trace] -> Trace
sprout = \traces -> \turtle -> foldl (grow id) turtle traces

straight : Float -> Trace
straight d (a, ((x,y)::ps)::pss) = (a, ((x + d * sin a, y + d * cos a)::(x,y)::ps)::pss)

transparent : Float -> Trace
transparent d (a, ((x,y)::ps)::pss) = (a, [(x + d * sin a, y + d * cos a)]::((x,y)::ps)::pss)

bend : Float -> Trace
bend d (a, pss) = (a+d, pss)

trace : Int -> Int -> Trace -> Turtle -> Element
trace = \w h trace -> collage w h . map (traced defaultLine) . snd . trace
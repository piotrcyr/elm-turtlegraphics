module TurtleGraphics where

{-| Library for creating [turtle graphics](http://en.wikipedia.org/wiki/Turtle_graphics)

# The turtle...
@docs Turtle, turtleAt

# ... and it's trace
@docs straight, transparent, bend

# joining and mixing traces
@docs join, grow, connect, sprout, (>>), (>.)

-}

import Graphics.Collage(collage)

infixl 1 >>
infixl 1 >.

{-| The turtle moves around leaving trace behing itself.
    At any given moment it faces the direction (given in radians).
    As turle's memory never fails it, it remembers bright as day
    all the path it travelled so far.
-}
type Turtle = (Float, [[(Float, Float)]])
type Trace = Turtle -> Turtle

{-| This puts a turtle at a given point.
-}
turtleAt : (Float, Float) -> Turtle
turtleAt = \pos -> (0, [[pos]])

{-| Joins two traces. The turtle finishes at the end of the resulting trace.
-}
join : Trace -> Trace -> Trace
join = flip (.)

(>>) : Trace -> Trace -> Trace
(>>) = join

{-| This is similar to `join`. It glues the traces together, but the turtle 
comes back to the point (leaving no trace behind this time) it would end after following
only the first trace. This is useful if you want to have multiple traces growing from a single point.
-}
grow : Trace -> Trace -> Trace
grow  tr1 tr2 = \turtle -> let (a, ((x,y)::ps)::pss) = tr1 turtle
                               (_, pss') = tr2 (a, ((x,y)::ps)::pss)
                           in (a, [(x,y)]::pss')

(>.) : Trace -> Trace -> Trace
(>.) = grow

{-| This is equivalent to `join`ing all the traces together one after another.
-}
connect : [Trace] -> Trace
connect = \traces -> \turtle -> foldl (<|) turtle traces

{-| This `grows` all the traces in the list from the starting point.
-}
sprout : [Trace] -> Trace
sprout = \traces -> \turtle -> foldl (grow id) turtle traces

{-| The turtle moves the given distance in the direction it's facing.
-}
straight : Float -> Trace
straight d (a, ((x,y)::ps)::pss) = (a, ((x + d * sin a, y + d * cos a)::(x,y)::ps)::pss)

{-| The turtle moves the given distance in the direction it's facing, but leaves no trace behind.
-}
transparent : Float -> Trace
transparent d (a, ((x,y)::ps)::pss) = (a, [(x + d * sin a, y + d * cos a)]::((x,y)::ps)::pss)

{-| The turtle turns a given angle.
-}
bend : Float -> Trace
bend d (a, pss) = (a+d, pss)

{-| This creates the Element presenting the trace from the starting point.
    You pass `width` and `height` of the element `trace` the turtle will follow and
    specify where the `turtle` begins relatively to the center of the resulting `Element`.
-}
trace : Int -> Int -> Trace -> Turtle -> Element
trace = \w h trace -> collage w h . map (traced defaultLine) . snd . trace
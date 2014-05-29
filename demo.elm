import TurtleGraphics(..)

tree : Int -> Float -> Float -> Float -> Float -> Trace
tree n x y tip ratio = case n of
                            1 -> straight tip
                            _ -> (straight <| tip * ratio * n) >.
                                         sprout [ bend x >> tree (n-1) x y tip ratio
                                                  , bend y >> tree (n-1) x y tip ratio
                                                  ]
                                         

koch : Int -> Float -> Float -> Trace
koch n len angle = case n of
                        0 -> straight len
                        _ -> connect [ koch (n-1) len angle
                                     , bend (-angle)
                                     , koch (n-1) len angle
                                     , bend (2*angle)
                                     , koch (n-1) len angle
                                     , bend (-angle)
                                     , koch (n-1) len angle
                                     ]

rdragon : Int -> Float -> Float -> Trace
rdragon n len angle = case n of
                           0 -> straight ( len)
                           _ -> connect [ bend (-angle)
                                        , rdragon (n-1) len angle
                                        , bend (2*angle)
                                        , ldragon (n-1) len angle
                                        , bend (-angle)
                                        ]
                           
ldragon n len angle = case n of
                           0 -> straight ( len)
                           _ -> connect [ bend (angle)
                                        , rdragon (n-1) len angle
                                        , bend (-2*angle)
                                        , ldragon (n-1) len angle
                                        , bend (angle)
                                        ]

spiral n len angle ratio = case n of
                                0 -> straight 0
                                _ -> connect [ bend angle
                                             , straight len
                                             , bend angle
                                             , straight len
                                             , bend angle
                                             , straight len
                                             , spiral (n-1) (ratio * len) angle ratio
                                             ]

main = flow down [ trace 200 200 (tree 7 (pi/6) (-pi/6) 4 1.001) (turtleAt (0,-50))
                 , trace 200 200 (spiral 200 200 (2*pi/5 - 0.02) (0.97)) (turtleAt (-100, 100))
                 , trace 200 200 (koch 5 2.8 (4*pi/9)) (turtleAt (0, -100))
                 , trace 200 200 (rdragon 13 (0.69) (pi/4.3)) (turtleAt (0, -60))
                 ]
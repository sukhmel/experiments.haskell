import Graphics.Gloss.Interface.Pure.Game

data Object = Moving { position :: Point
                     , velocity :: Point
                     , sprite   :: Picture
                     }
            | Static { position :: Point
                     , inside   :: Point -> Bool
                     , sprite   :: Picture
                     , bounce   :: Float
                     , normal   :: Point
                     }

data World = World { objects      :: [Object]
                   , acceleration :: Point 
                   }

draw :: World -> Picture
draw w = pictures
       . ((color red . polygon) [a, b, c, a] :)
       . map (\ obj
             ->  uncurry translate (position obj) (scale 0.5 0.5 $ sprite obj) )
       . objects
       $ w
    where a@(ax, ay) = acceleration w
          b@(bx, by) = (ay/m, -ax/m)
          c          = (-bx, -by)
          m          = sqrt (ax**2 + ay**2) / 5

handleKey :: Event -> World -> World
handleKey (EventKey c Down _ _) w = w'
    where (x,y) = acceleration w
          a'    = case c of
                    (Char 'a') -> (x - 2, y)
                    (Char 'd') -> (x + 2, y)
                    (Char 'w') -> (x, y + 2)
                    (Char 's') -> (x, y - 2)
                    _          -> (x, y)
          w'    = w { acceleration = a' }
handleKey _ w = w

-- bounce off walls, reducing speed to emulate dissipation
advanceStep :: Float -> World -> World
advanceStep t w = w { objects = obstacles ++ map process movables }
        where (ax, ay)  = acceleration w
              process m = case filter (flip inside $ position m) obstacles
                               of []    -> move m
                                  (s:_) -> calculate m s
              calculate m s = let b        = bounce s
                                  f        = inside s
                                  k        = 2 * (nx*vx + ny*vy)
                                  (nx, ny) = normal s
                                  (vx, vy) = velocity m
                                  pos'     = revert m f 1
                                  vel'     = (b*(vx - k*nx), b*(vy - k*ny))
                              in  move $ m { position = pos'
                                           , velocity = vel'
                                           }
              move m@(Moving{ position = (x,y), velocity = (vx, vy)})
                            = m{ position = ( x + vx*t,  y + vy*t)
                               , velocity = (vx + ax*t, vy + ay*t)
                               }
              population = objects w
              obstacles  = [ obj | obj@(Static {}) <- population]
              movables   = [ obj | obj@(Moving {}) <- population]

revert m f k = if f pos'
                  then pos
                  else revert m f (k/2)
    where pos'    = (px - k * vx / 2, py - k * vy / 2)
          pos     = (px - k * vx, py - k * vy)
          (vx,vy) = velocity m
          (px,py) = position m 

bigBlueLine = (color blue $ polygon [ (-3000,   0)
                                    , ( 3000,   0)
                                    , ( 3000, -10)
                                    , (-3000, -10)
                                    , (-3000,   0)
                                    ]
              )
wallH y f = Static (0, y) 
                   (f y . snd) 
                   bigBlueLine
                   0.9
                   (0, 1)
wallV x f = Static (x, 0) 
                   (f x . fst) 
                   (rotate 90 bigBlueLine)
                   0.9
                   (1, 0)
          
main :: IO ()
main = do ghost <- loadBMP "ghost.bmp"
          play ( InWindow "Boo!"
                          (800, 600)
                          (100, 100) )
               ( greyN 0.5 )
               30
               ( World [ Moving (0, 0)
                                (0, 0)
                                ghost
                       , wallH (-200) (>)
                       , wallH ( 200) (<)
                       , wallV (-200) (>)
                       , wallV ( 200) (<)
                       ]
                       (0, 0)
               )
               draw
               handleKey
               advanceStep
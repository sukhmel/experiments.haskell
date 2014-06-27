import Data.List
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

instance Show Object where
    show m@(Moving{}) = 'M' : '@' : show (position m)
    show s@(Static{}) = 'S' : '@' : show (position s) ++ "_|_" ++ show (normal s) ++ '&' : show (sprite s)
    
data World = World { objects      :: [Object]
                   , acceleration :: Point 
                   , viscosity    :: Point
                   , viscosityPow :: Point
                   }

draw :: World -> Picture
draw w = pictures
       . ((color red . polygon) [a, b, c, a] :)
       . map (\ obj
             ->  pictures . map (uncurry translate $ position obj)
               $ [ sprite obj
                 ])
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
                            = let (rx, ry) = resistance m
                              in  m{ position = ( x + t* vx,        y + t* vy      )
                                   , velocity = (vx + t*(ax - rx), vy + t*(ay - ry))
                                   }
              resistance m = let (vx, vy) = velocity m
                                 norm     = sqrt $ vx**2 + vy**2
                                 (cx, cy) = viscosity w
                                 (px, py) = viscosityPow w
                                 resist   = ( cx * vx * norm**(px - 1)
                                            , cy * vy * norm**(py - 1) )
                             in  resist
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
                   1.1
                   (0, 1)
wallV x f = Static (x, 0) 
                   (f x . fst) 
                   (rotate 90 bigBlueLine)
                   1.1
                   (1, 0)

triang [a, b, c] = let (ax, ay) = a
                       (bx, by) = b
                       (cx, cy) = c
                       o@(ox, oy) = ((ax + bx)/2, (ay + by)/2)
                       mv = (`sub` o)
                       a' = mv a
                       b' = mv b
                       c' = mv c
                       n = (ay - by, bx - ax)
                       p = (by - cy, cx - bx)
                       q = (cy - ay, ax - cx)
                       dot (x, y) (s, t) = x*s + y*t
                       sub (x, y) (s, t) = (x - s, y - t)
                       same normVector
                            linePoint
                            referencePoint
                            testedPoint = check testedPoint
                                       == check referencePoint
                                    where check = compare 0 . dot normVector . (`sub` linePoint)
                       inner v = same q a b v
                              && same n b c v
                              && same p c a v
                       (nx, ny) = n
                       norm = sqrt $ nx**2 + ny**2
                       
                   in  Static (ox, oy)
                              inner
                              (color blue $ polygon [ a', b', c', a' ])
                              0.95
                              (nx / norm, ny / norm)

repulsePoly as = map (triang . (++ [centre]) . take 2)
               . filter (not . null . drop 1)
               . tails
               $ as 
    where centre = average as (0,0) 0
          average ((x,y):rest) (xs, ys) n = average rest (xs + x, ys + y) (n + 1)
          average [] (x, y) n = (x/n, y/n)

main :: IO ()
main = do ghost <- loadBMP "ghost.bmp"
          play ( InWindow "Boo!"
                          (800, 600)
                          (100, 100) )
               ( greyN 0.5 )
               30
               ( World ( [ Moving (20, 20)
                                  (-10, -20)
                                  (scale 0.5 0.5 ghost)
                         , triang [ (-201, -200), ( 201, -200), (   0, -250) ]
                         , triang [ ( 200, -201), ( 200,  201), ( 250,    0) ]
                         , triang [ ( 200,  200), (-201,  200), (   0,  250) ]
                         , triang [ (-200,  201), (-200, -201), (-250,    0) ]
                         ]
                      ++ repulsePoly [ (  0,  50), ( 10,  10)
                                     , ( 50,   0), ( 10, -10)
                                     , (  0, -50), (-10, -10)
                                     , (-50,   0), (-10,  10)
                                     , (  0,  50)] )
                       (0, 0)
                       (0.0, 0.0)
                       (1, 1) -- Neutonian environment: viscosity force depends
                              -- on first power of speed.
               )
               draw
               handleKey
               advanceStep
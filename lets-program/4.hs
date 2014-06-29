{-# LANGUAGE TemplateHaskell #-}

import Data.List
import Control.Lens
import Graphics.Gloss.Interface.Pure.Game

data Object = Moving { _position :: Point
                     , _velocity :: Point
                     , _sprite   :: Picture
                     }
            | Static { _position :: Point
                     , _inside   :: Point -> Bool
                     , _sprite   :: Picture
                     , _bounce   :: Float
                     , _normal   :: Point
                     }

makeLenses ''Object
                     
instance Show Object where
    show m@(Moving{}) = 'M' : '@' : show (_position m)
    show s@(Static{}) = 'S' : '@' : show (_position s) ++ "_|_" ++ show (_normal s) ++ '&' : show (_sprite s)
    
data World = World { _objects      :: [Object]
                   , _acceleration :: Point 
                   , _viscosity    :: Point
                   , _viscosityPow :: Point
                   }

makeLenses ''World

draw :: World -> Picture
draw w = pictures
       . (++ [(color red . polygon) [a, b, c, a]])
       $ zipWith (\ pos spr -> pictures
                             . map (uncurry translate pos)
                             $ [spr])
                 ( w ^.. objects . traversed . position)
                 ( w ^.. objects . traversed . sprite)
    where a@(ax, ay) = view acceleration w
          b@(bx, by) = (ay/m, -ax/m)
          c          = (-bx, -by)
          m          = sqrt (ax**2 + ay**2) / 5

handleKey :: Event -> World -> World
handleKey (EventKey c Down _ _) w = w'
    where (x,y) = view acceleration w
          a'    = case c of
                    (Char 'a') -> (x - 2, y)
                    (Char 'd') -> (x + 2, y)
                    (Char 'w') -> (x, y + 2)
                    (Char 's') -> (x, y - 2)
                    _          -> (x, y)
          w'    = acceleration .~ a' $ w
handleKey _ w = w

-- bounce off walls, reducing speed to emulate dissipation
advanceStep :: Float -> World -> World
advanceStep t w = w { _objects = obstacles ++ map process movables }
        where (ax, ay)  = view acceleration w
              process m = case filter (flip _inside $ _position m) obstacles
                               of []    -> move m
                                  (s:_) -> calculate m s
              calculate m s = let b        = _bounce s
                                  f        = _inside s
                                  k        = 2 * (nx*vx + ny*vy)
                                  (nx, ny) = _normal s
                                  (vx, vy) = _velocity m
                                  pos'     = revert m f 1
                                  vel'     = (b*(vx - k*nx), b*(vy - k*ny))
                              in  move $ m { _position = pos'
                                           , _velocity = vel'
                                           }
              move m@(Moving{ _position = (x,y), _velocity = (vx, vy)})
                            = let (rx, ry) = resistance m
                              in  m{ _position = ( x + t* vx,        y + t* vy      )
                                   , _velocity = (vx + t*(ax - rx), vy + t*(ay - ry))
                                   }
              resistance m = let (vx, vy) = _velocity m
                                 norm     = sqrt $ vx**2 + vy**2
                                 (cx, cy) = _viscosity w
                                 (px, py) = _viscosityPow w
                                 resist   = ( cx * vx * norm**(px - 1)
                                            , cy * vy * norm**(py - 1) )
                             in  resist
              population = view objects w
              obstacles  = [ obj | obj@(Static {}) <- population]
              movables   = [ obj | obj@(Moving {}) <- population]

revert m f k = if f pos'
                  then pos
                  else revert m f (k/2)
    where pos'    = (px - k * vx / 2, py - k * vy / 2)
          pos     = (px - k * vx, py - k * vy)
          (vx,vy) = _velocity m
          (px,py) = _position m 

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

emptyBox (x0, y0)
         (x1, y1)
         ( w,  h) = repulsePoly [(x0, y0), (x1, y0), (x1, y0 + h), (x0, y0 + h), (x0, y0)]
                 ++ repulsePoly [(x0, y0), (x0, y1), (x0 + w, y1), (x0 + w, y0), (x0, y0)]
                 ++ repulsePoly [(x1, y1), (x0, y1), (x0, y1 - h), (x1, y1 - h), (x1, y1)]
                 ++ repulsePoly [(x1, y1), (x1, y0), (x1 - w, y0), (x1 - w, y1), (x1, y1)]

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
                         ]
                      ++ repulsePoly [ (  0,  50), ( 10,  10)
                                     , ( 50,   0), ( 10, -10)
                                     , (  0, -50), (-10, -10)
                                     , (-50,   0), (-10,  10)
                                     , (  0,  50)]
                      ++ emptyBox (-200, -200) ( 200, 200) ( 10, 10) )
                       (0, 0)
                       (0.0, 0.0)
                       (1, 1) -- Neutonian environment: viscosity force depends
                              -- on first power of speed.
               )
               draw
               handleKey
               advanceStep
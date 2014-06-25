import Graphics.Gloss.Interface.Pure.Game

data Object = Moving { position :: Point
                     , velocity :: Point
                     , sprite   :: Picture }
            | Static { position :: Point
                     , inside   :: Point -> Bool
                     , sprite   :: Picture }

data World = World { objects      :: [Object]
                   , acceleration :: Point }

draw :: World -> Picture
draw w = pictures
       . ((color red . line) [(0, 0), acceleration w] :)
       . map (\ (Object (x,y) (vx, vy) sprite)
             ->  translate x y (scale 0.5 0.5 sprite) )
       . objects
       $ w

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
advanceStep t w = w { objects = obstacles ++ map process population }
        where (ax, ay) = acceleration w
              process ( Moving ( x,  y) 
                               (vx, vy)
                                sprite)
                       = let vx' = if x < -400 || x > 400 then -vx*0.7 else vx
                             vy' = if y < -300 || y > 300 then -vy*0.7 else vy
                         in  Object ( x + vx'*t,  y + vy'*t)
                                    (vx' + ax*t, vy' + ay*t) sprite
              population = objects w
              obstacles  = [ obj | obj@(Static {}) <- population]
              movables   = [ obj | obj@(Moving {}) <- population]


revert w f = 


main :: IO ()
main = do ghost <- loadBMP "ghost.bmp"
          play ( InWindow "Boo!"
                          (800, 600)
                          (100, 100) )
               ( greyN 0.5 )
               30
               ( World [Moving (0, 0) (0, 0) ghost] (0, 0) )
               draw
               handleKey
               advanceStep
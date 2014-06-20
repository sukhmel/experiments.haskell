import Graphics.Gloss.Interface.Pure.Game

data World = World Point

draw :: Picture -> (World -> Picture)
draw sprite (World (x, y)) = translate x y (scale 0.5 0.5 sprite)

handleKey :: Event -> World -> World
handleKey (EventKey c Down _ _) w@(World (x, y)) = w'
    where w' = case c of
                    (Char 'a') -> World (x - 10, y)
                    (Char 'd') -> World (x + 10, y)
                    (Char 'w') -> World (x, y + 10)
                    (Char 's') -> World (x, y - 10)
                    _          -> w
handleKey _ w = w

main :: IO ()
main = do ghost <- loadBMP "ghost.bmp"
          play ( InWindow "Boo!"
                          (600, 400)
                          (50, 50) )
               ( greyN 0.5 )
               30
               ( World (0, 0) )
               ( draw ghost ) 
               handleKey
               (const id)
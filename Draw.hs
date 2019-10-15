module Draw 
(fd
,bk
,rt
,lf
,penup
,pendown
,repeat
,save
,clear
,penColor
,extractImage 
,Turtle
,resetPosition
,getPrintables
,red
,blue
,teal
,aqua
,indianred
,white
) where

import Graphics.GD
import Prelude hiding (repeat)

type Acc = (Int, Int, Int, Bool, Color, IO Image)

newtype Turtle t = T (Acc -> (t, Acc))

--------------------------------------------------------------------------------------------
-- Functor/Applicative/Monad definitions
--------------------------------------------------------------------------------------------
move :: Turtle t -> Acc -> (t, Acc)
move (T st) x =  st x

instance Functor Turtle where
  --fmap (a->b) -> Turtle a -> Turtle b
  fmap g st = T (\s -> let (x,s') = move st s in (g x, s'))

instance Applicative Turtle where 
  --pure :: a -> Turtle a 
  pure x = T (\s -> (x,s))

 
  stf <*> stx = T (\s ->
                let (f,s') = move stf s
                    (x, s'') = move stx s'
                    in   (f x, s'')) 

instance Monad Turtle where 
     st >>= f = T (\s -> let (x,s') = move st s in move (f x) s')


----------------------------------------------------------------------------------------------
--operations
---------------------------------------------------------------------------------------------
--get the image from the tuple, start at the center with -90 degrees
extractImage :: Turtle Int -> IO Image 
extractImage st = getImage (snd $ move st (200,200,-90,True,(setColor 50 50 50), newImage (400, 400)))


--move forward by a units
fd :: Int -> Turtle Int
fd a = T (\(x,y,o,p,color,image) -> case p of
                   False -> (a, ((getX x a o), (getY y a o), o, p, color, image))
                   True -> (a, ((getX x a o), (getY y a o), o, p, color, (draw x y (getX x a o) (getY y a o) color image)))) 

--move backward by a units
bk :: Int -> Turtle Int 
bk a = fd (-a)
--bk a = T (\(x,y,o,p,color,image) -> case p of 
--                  False -> (a, (x,y,o,p, color,image))
--                  True -> (a, (x,y,o,p, color, do 
--                               im <- image
--                               drawLine (x,y) (x,y) color im
--                               return im)))


-- turn left
lf :: Int -> Turtle Int 
lf a = T(\(x,y,o,p,color,image)  -> (a,( x, y, (mod (o + 360 - mod a 360) 360), p, color, image)))

-- turn right
rt :: Int -> Turtle Int 
rt a = T (\(x,y,o,p,color,image) -> (a,(x, y, (mod (o + a) 360), p, color, image)))

-- pen down
pendown ::  Turtle Int
pendown  = T(\(x,y,o,p,color,image) -> (x, (x,y,o,True,color,image)))

--pick the pen up
penup ::Turtle Int
penup = T(\(x,y,o,p,color,image) -> (x, (x,y,o,False,color,image)))

-----------------------------------------------------------------------------------------------------------------------------------

-- Tried to resize, resizeImage not working, and makes my stuff slow, didnt use it. for future use

---------------------------------------------------------------------------------------------------------------------------------


resizePossible :: Int -> Int -> Int -> Int -> Color -> IO Image -> IO Image 
resizePossible x y x1 y1 c image = do
          im <- image 
          s <- imageSize im
          if (fst s * snd s) < (x1 * y1) then do  
              -- let image1 = resizeImage (fst s * 2) (snd s * 2) im
              let image1 = newImage (fst s + x1, snd s + y1)
              im1 <- image1
              let x2 = round((fromIntegral (fst s) + fromIntegral x1) / fromIntegral 2)
              let y2 = round ((fromIntegral (snd s) + fromIntegral y1)/ fromIntegral 2)
              copyRegion (0,0) s im (x2, y2) im1
              draw x y x1 y1 c image1
              --return im1
          else if (fst s < x1) then do
               --let image2 = resizeImage (fst s *2) y1 im
               let image2 = newImage (fst s + x1, y1)
               im2 <- image2
               let x2 = round((fromIntegral (fst s) + fromIntegral x1) / fromIntegral 2)
               copyRegion (0,0) s im (x2, y1) im2
               draw x y x1 y1 c image2
               --return im2
          else if (snd s < y1) then do 
               let image3 = newImage (x1, snd s * 2)
               let y2 = round ((fromIntegral (snd s) + fromIntegral y1)/ fromIntegral 2)
               im3 <- image3
               copyRegion (0,0) s im (x1, y2) im3
               draw x y x1 y1 c image3
               --return im3 
          else do 
             let image4 = newImage (600, 600)
             im4 <- image4
             copyRegion (0,0) s im (300, 300) im4 
             draw x y x1 y1 c image4
-------------------------------------------------------------------------------------------------------------------------------------

-- draw line on the image. Called in the forward function
draw :: Int -> Int -> Int -> Int ->Color -> IO Image -> IO Image
draw x y x1 y1 c image = do 
          im <- image
          drawLine (x, y) (x1, y1) c im
          return im

--- repeat number of operations n times
repeat :: Int -> [Turtle Int] -> Turtle Int
repeat 0 ops = T (\(x,y,o,p,color,image) -> (x, (x,y,o,p, color,image)))
repeat n ops = do 
               performOps(ops)
               repeat (n-1) ops

--helper function for repeat
performOps :: [Turtle Int] -> Turtle Int
performOps [] = T (\(x,y,o,p,color,image) -> (x, (x,y,o,p, color,image)))
performOps (x:xs)  =  do 
                      x 
                      performOps xs
                   

-- clears the canvas
clear :: Turtle Int
clear = T (\(x,y,o,p,color, image) -> (0,(200,200,(-90),False,color, (newImage (400,400)))))


--save image as jpg file
save :: String -> Turtle Int
save f = T (\(x,y,o,p,color,image) -> (x, (x,y,o,p,color, do
                       im <- image
                       saveJpegFile 50 f im 
                       return im)))

-- pen color 
penColor :: Color -> Turtle Int 
penColor c = T (\(x, y, o, p, color, image) -> (x, (x, y, o, p, c, image)))

-------------------------------------------------------------------------------------------------------------
--helper functions to calculate new position, called by forward
getX :: Int -> Int -> Int -> Int 
getX x d angle = round (fromIntegral x + fromIntegral d * cos (fromIntegral angle/ fromIntegral 180 * pi))


getY :: Int -> Int -> Int -> Int
getY y d angle = round (fromIntegral y + fromIntegral d * sin (fromIntegral angle/ fromIntegral 180 * pi))

---------------------------------------------------------------------------------------------------------------


setColor :: Int -> Int -> Int -> Color
setColor r g b = rgb r b g

-- take the turtle to where it started
resetPosition :: Turtle Int
resetPosition = T (\(x,y,o,p,color,image) -> (x, (200, 200, o, p, color, image)))


--get the image out the tuple, gets called by extractimage function
getImage :: (Int, Int, Int, Bool, Color, IO Image) -> IO Image
getImage (_,_,_,_,_,i) = i

getPrintables :: (Int, Int, Int, Bool, Color,IO Image) -> (Int, Int, Int, Bool)
getPrintables (x,y,o,p,_,_) = (x,y,o,p) 
-------------------------------------------------------------------------------------------------------------------
--colors 

red :: Color
red = rgb 255 0 0

aqua :: Color
aqua = rgb 0 255 255

teal :: Color
teal  = rgb 0 128 128

blue :: Color
blue = rgb 0 0 255

indianred :: Color
indianred = rgb 205 92 92

white :: Color
white = rgb 255 255 255 
-----------------------------------------------------------------------------------------------------------------

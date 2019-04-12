import Draw
import Prelude hiding (repeat)

perform :: Turtle Int
perform = do 
    clear 
    pendown
    --clear
    --penColor red 
    repeat 180 [fd 100, rt 30, fd 20, penColor red, lf 60, fd 50, rt 30,penup, resetPosition, pendown, rt 2, penColor indianred]
    save "ninja.jpg"

main :: IO ()  
main = do
     let a  = extractImage perform
     image <- a
     return ()


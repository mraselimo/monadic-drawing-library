import Draw
import Prelude hiding (repeat)

perform :: Turtle Int
perform = do 
    clear 
    pendown

    penColor blue 
    repeat 50 [fd 50, lf 123]
    --clear
    penColor red 
    repeat 50 [fd 100, lf 123]
    save "painter.jpg"

main :: IO ()  
main = do
     let a  = extractImage perform
     image <- a
     return ()


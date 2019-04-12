import Draw
import Prelude hiding (repeat)

perform :: Turtle Int
perform = do 
    clear 
    pendown
    --clear
    penColor white
    penup
    rt 45
    fd 90
    rt 135
    pendown
    
    repeat 120 [fd 200, rt 61, fd 200, rt 61, fd 200, rt 61, fd 200, rt 61, fd 200, rt 61, fd 200, rt 61, rt 11]
    save "wtf.jpg"

main :: IO ()  
main = do
     let a  = extractImage perform
     image <- a
     return ()


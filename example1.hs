import Draw
import Prelude hiding (repeat)

perform :: Turtle Int
perform = do 
    clear 
    pendown
   
    penColor aqua
    fd 60
    rt 72
    fd 60
    rt 72
    fd 60
    --penup
    rt 72
    fd 60
    rt 72
    fd 60
    --penup      --rt 90
    save "pentagon.jpg"

main :: IO ()  
main = do
     let im  = extractImage perform
     image <-  im
     return ()  


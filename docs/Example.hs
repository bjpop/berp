module Main where

import Object as O
import Data.IORef

{-
x = 5

def fac (x):
    if x == 0:
       return 1 
    else:
       return x * fac(x-1) 

print fac (x)
-}

fac = mkProcedure fac_proc

fac_proc :: [O.Object] -> IO O.Object 
fac_proc [arg1] = do
    x <- newIORef arg1
    x_obj <- readIORef x
    v1 <- O.lookup x_obj "__eq__"
    v2 <- O.call v1 [x_obj, O.mkInteger 0]
    if O.isTrue v2 
       then return (mkInteger 1)
       else do  
           x_obj <- readIORef x
           v3 <- O.lookup x_obj "__sub__"
           v4 <- O.call v3 [x_obj, O.mkInteger 1]
           v5 <- O.call fac [v4]
           v6 <- O.lookup x_obj "__mul__"
           O.call v6 [x_obj, v5]

init :: IO Object 
init = do
   x <- newIORef undefined
   writeIORef x (O.mkInteger 2000)    -- x = 5
   v5 <- readIORef x
   v6 <- O.call fac [v5] 
   O.print v6
   return O.mkNone

main = Main.init

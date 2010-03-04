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
fac_proc [x] = do
    v1 <- O.lookup x "__eq__"
    v2 <- O.call v1 [x, O.mkInteger 0]
    if O.isTrue v2 
       then return (mkInteger 1)
       else do  
           v3 <- O.lookup x "__sub__"
           v4 <- O.call v3 [x, O.mkInteger 1]
           v5 <- O.call fac [v4]
           v6 <- O.lookup x "__mul__"
           O.call v6 [x, v5]

init :: IO Object 
init = do
   let x = (O.mkInteger 2000)    -- x = 5
   v6 <- O.call fac [x] 
   O.print v6
   return O.mkNone

main = Main.init

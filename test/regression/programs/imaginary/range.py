# Based on the Haskell program at the bottom, as seen on the haskell-cafe mailing list.
# 28/3/10. Transliterated into Python by Bernie.

#import resource
#import sys

# Increase max stack size from 8MB to 512MB
#resource.setrlimit(resource.RLIMIT_STACK, (2**29,-1))
#sys.setrecursionlimit(10**6)

# rangeK :: Int -> Int -> Int -> Int -> Int
# rangeK i j k acc
#      | k < 1000 =
#          if i * i + j * j + k * k `mod` 7 == 0
#          then rangeK i j (k+1) (acc+1)
#          else rangeK i j (k+1) acc
#      | otherwise = acc

def rangeK (i, j, k, acc):
    print ((i,j,k, acc))
    if k < 1000:
        if ((i * i) + (j * j) + ((k * k) % 7)) == 0:
            return rangeK (i, j, k+1, acc+1)
        else: 
            return rangeK (i, j, k+1, acc)
    else: 
       return acc

# rangeJ :: Int -> Int -> Int -> Int
# rangeJ i j acc
#      | j < 1000 = rangeJ i (j+1) (acc + rangeK i j 0 0)
#      | otherwise = acc

def rangeJ (i, j, acc):
    if j < 1000: 
        return rangeJ (i, j+1, acc + rangeK (i, j, 0, 0))
    else: 
        return acc

# rangeI :: Int -> Int -> Int
# rangeI i acc
#      | i < 1000 = rangeI (i+1) (acc + (rangeJ i 0 0))
#      | otherwise = acc

def rangeI (i, acc):
    if i < 1000:
        return rangeI(i+1, acc + rangeJ(i, 0, 0))
    else: 
        return acc

# main :: IO ()
# main = print $ rangeI 0 0

print(rangeI(0, 0))

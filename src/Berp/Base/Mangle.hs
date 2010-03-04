module Berp.Base.Mangle (mangle, deMangle) where

import Data.List (isPrefixOf)

sourcePrefix = "_s_"

mangle :: String -> String
mangle = (sourcePrefix ++)

deMangle :: String -> String
deMangle str
   | sourcePrefix `isPrefixOf` str = drop (length sourcePrefix) str
   | otherwise = str

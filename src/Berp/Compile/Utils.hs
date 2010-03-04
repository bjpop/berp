module Berp.Compile.Utils where

unsupported :: String -> a
unsupported str = error $ "berp unsupported. " ++ str

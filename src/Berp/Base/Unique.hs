{-# LANGUAGE MagicHash #-}
-- Same as the version in GHC but with added show instance.

module Berp.Base.Unique (
   -- * Unique objects
   Unique (..),         -- instance (Eq, Ord)
   newUnique,           -- :: IO Unique
   hashUnique           -- :: Unique -> Int
 ) where

import Prelude

import Control.Concurrent.MVar
import System.IO.Unsafe (unsafePerformIO)

import GHC.Base
import GHC.Num

-- | An abstract unique object.  Objects of type 'Unique' may be
-- compared for equality and ordering and hashed into 'Int'.
newtype Unique = Unique { uniqueInteger :: Integer } deriving (Eq,Ord)

-- not safe, but needed for printing purposes. 
instance Show Unique where
   show (Unique i) = show i

{-# NOINLINE uniqSource #-}
uniqSource :: MVar Integer
uniqSource = unsafePerformIO (newMVar 0)

-- | Creates a new object of type 'Unique'.  The value returned will
-- not compare equal to any other value of type 'Unique' returned by
-- previous calls to 'newUnique'.  There is no limit on the number of
-- times 'newUnique' may be called.
newUnique :: IO Unique
newUnique = do
   val <- takeMVar uniqSource
   let next = val+1
   putMVar uniqSource next
   return (Unique next)

-- | Hashes a 'Unique' into an 'Int'.  Two 'Unique's may hash to the
-- same value, although in practice this is unlikely.  The 'Int'
-- returned makes a good hash key.
hashUnique :: Unique -> Int
hashUnique (Unique i) = I# (hashInteger i)

{-# LANGUAGE DeriveDataTypeable #-}

module Berp.Base.Exception (RuntimeError (..), module Except) where

import Control.Exception.Extensible as Except
import Data.Typeable

data RuntimeError
   = UncaughtException String
   deriving (Typeable, Show)

instance Exception RuntimeError

{-# LANGUAGE DeriveDataTypeable #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Berp.Base.Exception
-- Copyright   : (c) 2010 Bernie Pope
-- License     : BSD-style
-- Maintainer  : florbitous@gmail.com
-- Stability   : experimental
-- Portability : ghc
--
-- Custom exception values for Berp. Note: these do not necessarily 
-- correspond to Python's exception values. Those are encoded elsewhere.
-- The exceptions in this module are just for the internal use of berp.
--
-----------------------------------------------------------------------------


module Berp.Base.Exception (RuntimeError (..), module Except) where

import Control.Exception.Extensible as Except
import Data.Typeable

data RuntimeError
   = UncaughtException String
   deriving (Typeable, Show)

instance Exception RuntimeError

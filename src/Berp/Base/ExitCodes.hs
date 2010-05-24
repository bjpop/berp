-----------------------------------------------------------------------------
-- |
-- Module      : Berp.Base.ExitCodes
-- Copyright   : (c) 2010 Bernie Pope
-- License     : BSD-style
-- Maintainer  : florbitous@gmail.com
-- Stability   : experimental
-- Portability : ghc
--
-- Exit codes for the various ways berp can "fail".
--
-----------------------------------------------------------------------------

module Berp.Base.ExitCodes (uncaughtExceptionError) where

import System.Exit (ExitCode (..))

uncaughtExceptionError :: ExitCode
uncaughtExceptionError = ExitFailure 1

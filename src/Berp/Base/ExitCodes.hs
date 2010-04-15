module Berp.Base.ExitCodes (uncaughtExceptionError) where

import System.Exit (ExitCode (..))

uncaughtExceptionError :: ExitCode
uncaughtExceptionError = ExitFailure 1

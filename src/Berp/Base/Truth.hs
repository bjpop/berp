-----------------------------------------------------------------------------
-- |
-- Module      : Berp.Base.Truth
-- Copyright   : (c) 2010 Bernie Pope
-- License     : BSD-style
-- Maintainer  : florbitous@gmail.com
-- Stability   : experimental
-- Portability : ghc
--
-- Implementation of the truth predicate on Python objects.
--
-----------------------------------------------------------------------------

module Berp.Base.Truth (truth) where

import Berp.Base.SemanticTypes (Object (..))

-- XXX incomplete
truth :: Object -> Bool
truth obj@(Bool { object_bool = b }) = b
truth obj@(Integer { object_integer = i }) = i /= 0
truth None = False 
truth other = False

{-
   From the Python Docs: http://docs.python.org/library/stdtypes.html#truth-value-testing

   Any object can be tested for truth value, for use in an if or while condition or as operand of 
   the Boolean operations below. The following values are considered false:

   None

   False

   zero of any numeric type, for example, 0, 0L, 0.0, 0j.

   any empty sequence, for example, '', (), [].

   any empty mapping, for example, {}.

   instances of user-defined classes, if the class defines a __nonzero__() or __len__() method, when 
   that method returns the integer zero or bool value False. [1]

   All other values are considered true — so objects of many types are always true.

   Operations and built-in functions that have a Boolean result always return 0 or False 
   for false and 1 or True for true, unless otherwise stated. 
   (Important exception: the Boolean operations or and and always return one of their operands.)
-}

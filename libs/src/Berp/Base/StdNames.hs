-----------------------------------------------------------------------------
-- |
-- Module      : Berp.Base.StdNames
-- Copyright   : (c) 2010 Bernie Pope
-- License     : BSD-style
-- Maintainer  : florbitous@gmail.com
-- Stability   : experimental
-- Portability : ghc
--
-- Hashed versions of standard Python identifiers.
--
-----------------------------------------------------------------------------

module Berp.Base.StdNames where

import Berp.Base.Hash (hashedStr, Hashed)

specialAddName :: Hashed String
specialAddName = hashedStr "__add__"
specialAndName :: Hashed String
specialAndName = hashedStr "__and__"
specialEqName :: Hashed String
specialEqName = hashedStr "__eq__"
specialGetItemName :: Hashed String
specialGetItemName = hashedStr "__getitem__"
specialGeName :: Hashed String
specialGeName = hashedStr "__ge__"
specialGtName :: Hashed String
specialGtName = hashedStr "__gt__"
specialLeName :: Hashed String
specialLeName = hashedStr "__le__"
specialLtName :: Hashed String
specialLtName = hashedStr "__lt__"
specialCmpName :: Hashed String
specialCmpName = hashedStr "__cmp__"
specialMulName :: Hashed String
specialMulName = hashedStr "__mul__"
specialDivName :: Hashed String
specialDivName = hashedStr "__div__"
specialOrName :: Hashed String
specialOrName = hashedStr "__or__"
specialSetItemName :: Hashed String
specialSetItemName = hashedStr "__setitem__"
specialStrName :: Hashed String
specialStrName = hashedStr "__str__"
specialSubName :: Hashed String
specialSubName = hashedStr "__sub__"
specialIterName :: Hashed String
specialIterName = hashedStr "__iter__"
specialNextName :: Hashed String
specialNextName = hashedStr "__next__"
specialModName :: Hashed String
specialModName = hashedStr "__mod__"
specialDocName :: Hashed String
specialDocName = hashedStr "__doc__"
specialInitName :: Hashed String
specialInitName = hashedStr "__init__"
mroName :: Hashed String
mroName = (hashedStr "mro")
specialHashName :: Hashed String
specialHashName = hashedStr "__hash__"
appendName :: Hashed String
appendName = hashedStr "append"
addName :: Hashed String
addName = hashedStr "add"

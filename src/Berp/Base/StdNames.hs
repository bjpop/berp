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

addName :: Hashed String
addName = (hashedStr "__add__")
andName :: Hashed String
andName = (hashedStr "__and__")
eqName :: Hashed String
eqName = (hashedStr "__eq__")
getItemName :: Hashed String
getItemName = (hashedStr "__getitem__")
geName :: Hashed String
geName = (hashedStr "__ge__")
gtName :: Hashed String
gtName = (hashedStr "__gt__")
leName :: Hashed String
leName = (hashedStr "__le__")
ltName :: Hashed String
ltName = (hashedStr "__lt__")
cmpName :: Hashed String 
cmpName = (hashedStr "__cmp__")
mulName :: Hashed String
mulName = (hashedStr "__mul__")
divName :: Hashed String
divName = (hashedStr "__div__")
orName :: Hashed String
orName = (hashedStr "__or__")
setItemName :: Hashed String
setItemName = (hashedStr "__setitem__")
strName :: Hashed String
strName = (hashedStr "__str__")
subName :: Hashed String
subName = (hashedStr "__sub__")
iterName :: Hashed String
iterName = (hashedStr "__iter__")
nextName :: Hashed String
nextName = (hashedStr "__next__")
modName :: Hashed String
modName = (hashedStr "__mod__")
docName :: Hashed String
docName = (hashedStr "__doc__")
initName :: Hashed String
initName = (hashedStr "__init__")
mroName :: Hashed String
mroName = (hashedStr "mro")
hashName :: Hashed String
hashName = (hashedStr "__hash__")
appendName :: Hashed String
appendName = (hashedStr "append")

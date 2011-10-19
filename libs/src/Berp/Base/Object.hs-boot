module Berp.Base.Object 
   (lookupAttribute, lookupSpecialAttribute, lookupAttributeMaybe
   , typeOf, dictOf, identityOf, objectEquality
   , isIterator ) where

import Berp.Base.SemanticTypes (Object, Eval, Identity)
import Berp.Base.Hash (Hashed)

typeOf :: Object -> Eval Object
identityOf :: Object -> Identity
dictOf :: Object -> Maybe Object
lookupAttribute :: Object -> Hashed String -> Eval Object
lookupSpecialAttribute :: Object -> Hashed String -> Eval Object
lookupAttributeMaybe :: Object -> Hashed String -> Eval (Maybe Object)
objectEquality :: Object -> Object -> Eval Bool
isIterator :: Object -> Eval Bool

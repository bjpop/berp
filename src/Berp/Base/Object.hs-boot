module Berp.Base.Object 
   (lookupAttribute, lookupAttributeMaybe, 
    typeOf, dictOf, identityOf, objectEquality) where

import Berp.Base.SemanticTypes (Object, Eval)
import Berp.Base.Identity (Identity)
import Berp.Base.Hash (Hashed)

typeOf :: Object -> Object
identityOf :: Object -> Identity
dictOf :: Object -> Maybe Object 
lookupAttribute :: Object -> Hashed String -> Eval Object
lookupAttributeMaybe :: Object -> Hashed String -> IO (Maybe Object)
objectEquality :: Object -> Object -> Eval Bool

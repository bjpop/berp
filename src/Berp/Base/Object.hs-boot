module Berp.Base.Object 
   (lookupAttribute, lookupAttributeMaybe, 
    typeOf, dictOf, identityOf, hasAttribute, objectEquality) where

import Berp.Base.SemanticTypes (Object, Eval)
import Berp.Base.Identity (Identity)
import Berp.Base.Hash (Hashed)

typeOf :: Object -> Object
identityOf :: Object -> Identity
dictOf :: Object -> Maybe Object 
lookupAttribute :: Object -> Hashed String -> IO Object
lookupAttributeMaybe :: Object -> Hashed String -> IO (Maybe Object)
hasAttribute :: Object -> Hashed String -> IO Bool
objectEquality :: Object -> Object -> Eval Bool

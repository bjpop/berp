module Berp.Base.StdTypes.ObjectBase (objectBase) where

import Berp.Base.SemanticTypes (Object)
import {-# SOURCE #-} Berp.Base.StdTypes.Object (object)
import {-# SOURCE #-} Berp.Base.StdTypes.Tuple (tuple)

-- | Most (all?) of the standard types have "object" as their (only) base
-- class. In Python the base classes are stored as a tuple of objects.
-- Since this is shared by many of the standard types it makes sense
-- to define it once, instead of making many copies.
objectBase :: Object
objectBase = tuple [object]

{-# OPTIONS_GHC -XTypeSynonymInstances #-}
module Berp.Base.Identity (Identity, newIdentity) where

import Berp.Base.Unique
import Berp.Base.Hash (Hash (..))
import Berp.Base.LiftedIO (MonadIO, liftIO)

type Identity = Unique

newIdentity :: MonadIO m => m Unique
newIdentity = liftIO newUnique

instance Hash Identity where
   hash = hashUnique

{- 

Some comments on this important module.

All Python objects have an identity which is unique to the object.
The identity of an object can be obtained by the primitive function id().

id is a "built in type or function". 

It returns an integer (class 'int'). According to the __doc__ for id
in Python 3.0 it says:

   id(...)
       id(object) -> integer
    
       Return the identity of an object.  This is guaranteed to be unique among
       simultaneously existing objects.  (Hint: it's the object's memory address.)

There's probably Python code out there which depends on the result being
an actual integer. But it would be nicer if it returned an abstract type.

There's also a builtin called hash():

   hash(...)
       hash(object) -> integer
    
       Return a hash value for the object.  Two objects with the same value have
       the same hash value.  The reverse is not necessarily true, but likely.
 
In some cases the hash function uses the identity of the object to obtain the hash
value.

The hash is quite useful, particularly because it is used to allow an object to be
a key in a dictionary. 

CPython's garbage collector does not move objects allocated on the heap.
This means it can use the address of the object as its
identity. Obviously this is problematic if we want to use GHC's collector which
does move objects.

Thus we must generate a unique identity for all objects when they are constructed.

A couple of important considerations:
   a) The scheme must scale. We should not have any limit on the number of
      identities that we can generate. 
   b) As computation time goes on we'd like to keep
      a handle on the size of individual identities. A constant size would be
      ideal, but we might allow for growth in the size of the identity value
      if it has reasonable asymptotic behaviour.
   c) It should work well with threads. Global counters must be atomically
      updated. 
   d) It is better if the scheme is portable (does not rely on deep GHC magic). 
   e) Should be fast.

A couple of options for implementation:

   1) A global mutable Integer counter protected by an MVar.
         - Satisfies a).
         - Size of counter grows logaithmically, but very slowly, so may be
           practical for the vast majority of applications. So probably 
           satisfies b).
         - Will work with threads, but at what cost? Each time an object
           is constructed the running thread must take the lock on the MVar,
           increment the counter, and release the lock. Incrementing an 
           Integer is not trivial, so there may be lock contention. 
           Probably satisfies c), but the significance of the time costs
           are unknown. 
         - MVars are not too magical, so probably satisfies d).

   2) A Stable Name.
         - Satisfies a). The number of stable names is only limited to the
           number of objects in memory (I think).
         - Satisfies b). The size of a stable name is constant. (good).
         - Satisfies c). I don't think there is any issue with thread 
           contention.
         - I think stable names are part of the FFI, so should be portable.
           Better check this. However, I don't think they work with parallel
           Haskell at present. Is this important? Hard to say.

   Regarding the speed of each method: it is hard to say without measuring
   them on real programs. My intution is that Stable Names have some advantage
   in multi-threaded programs because they don't go via MVars (or maybe they
   do, if the stable name table is locked in the runtime - better check this).
   I'm also a bit concerned that Stable Names were not designed to support a very
   large number of objects, and so may perform badly on Python programs which
   allocate many objects. 
-}

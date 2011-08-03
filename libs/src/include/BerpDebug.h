/*
 Copyright   : (c) 2010 Bernie Pope
 License     : BSD-style
 Maintainer  : florbitous@gmail.com 

 Debugging CPP macros.

 LIO refers to the LiftedIO library. This allows us to use putStrLn in any
 MonadIO context.
*/

#ifdef DEBUG
#define BELCH(str) (LIO.putStrLn (str))
#define IF_DEBUG(action) (action)
#else
#define BELCH(str)
#define IF_DEBUG(action)
#endif

#ifdef DEBUG
#define BELCH(str) (LIO.putStrLn (str))
#define IF_DEBUG(action) (action)
#else
#define BELCH(str)
#define IF_DEBUG(action)
#endif

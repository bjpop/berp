#ifdef DEBUG
#define BELCH_IO(str) (putStrLn (str))
#define BELCH_EVAL(str) (liftIO (putStrLn (str)))
#define IF_DEBUG(action) (action)
#else
#define BELCH_IO(str)
#define BELCH_EVAL(str)
#define IF_DEBUG(action)
#endif

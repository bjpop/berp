def foo():
   def loop(x): return loop(x)
   return loop(3)

foo()

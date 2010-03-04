def f(x):
    def g():
       nonlocal y
       if False:
          return x
       else:
          return y 
    return g

def h(foo):
   y = 12
   print (foo())

h(f(True))

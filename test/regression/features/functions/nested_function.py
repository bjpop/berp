def f(x):
   def g(x):
      return x + 1
   return g(x)

print(f(11))

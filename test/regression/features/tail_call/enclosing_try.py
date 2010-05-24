def f(x):
   try:
      return g(x+1)
   except:
      print("caught exception")

   print("after handler")
   return x

def g(y):
    return y / 0

print(f(12))

def f(x):
    y = 100
    def g():
       if False:
          return x
       else:
          return y 
    y = 12
    return g()
print(f(3)) 

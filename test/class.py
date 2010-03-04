class Foo (object):
   x = 5
   def __init__(self, z):
     self.foo = z 
   print(x)
   # pass

x = Foo(42)
x.bar = True 
print(x.bar)
print(x.foo)
print(x.x)

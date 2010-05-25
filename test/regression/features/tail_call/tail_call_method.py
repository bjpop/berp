class C(object):
   def foo(self): return 12

def f():
   x = C()
   return x.foo()

print(f())

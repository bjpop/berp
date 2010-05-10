class A(object):
   def foo(self):
      return("from A") 

class B(A): pass

class C(B): pass

class D(C):
   def foo(self):
      return("from D")

print(A().foo())
print(B().foo())
print(C().foo())
print(D().foo())

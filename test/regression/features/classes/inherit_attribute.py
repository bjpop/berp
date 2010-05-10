class A(object):
   def __init__(self):
      self.x = 12

class B(A): pass

class C(B): pass

class D(C):
   def __init__(self):
      self.x = 5

print(A().x)
print(B().x)
print(C().x)
print(D().x)

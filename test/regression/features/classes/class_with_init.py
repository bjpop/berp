class C(object):
   def __init__(self):
      self.x = 3 

print(C().x)

class C(object):
   def __init__(self,x):
      self.x = x 

print(C(48).x)

class C(object):
   def __init__(self,x,y):
      self.x = (x,y) 

print(C(True,"fred").x)

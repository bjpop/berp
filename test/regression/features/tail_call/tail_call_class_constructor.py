# a tail call to a class constructor

class C(): pass

def f():
   g ()
   return 0 

def g():
   if True:
      return C()
   # should never get here
   print("should not be here")

print(f())

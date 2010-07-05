def f():
   yield 1
   yield 2
   yield 3

x,y,z = f()
print(x,y,z)

# This should be an error (it is in berp and python, but we don't yet test for errors).
#def f():
#   yield (1,2,3)

#x,y,z = f()
#print(x,y,z)

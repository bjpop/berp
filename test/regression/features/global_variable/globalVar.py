x = 5
print(x)

def f():
   x = 2
   print(x)

def g():
    global x
    x = 1

f()
g()
print(x)

def f(y):
   global x
   y = y + 3 + x

x = 5
f(7)
print(x)

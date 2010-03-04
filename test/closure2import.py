def f():
    def g():
        return y 
    return g
from closure_y import *
print(f()()) 

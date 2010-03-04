def f():
    def g():
        def h():
           return y 
        return h
    return g
y=12
print(f()()()) 

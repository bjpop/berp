def f():
    def g():
        return y 
    return g
y=12
print(f()()) 

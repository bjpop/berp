def f():
    x = 5
    def g(y):
        print (x + y)
    g(1)
    x = 6
    g(1)
    x = 7
    g(1)

f()

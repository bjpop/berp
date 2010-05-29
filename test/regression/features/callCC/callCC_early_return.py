def f(g):
    g(True)
    return False

print(f(lambda x: x))
print(callCC(f))

def fac(n, acc):
    if n == 0:
       return acc
    else:
       return fac(n-1, n*acc)

print(fac(1000, 1))

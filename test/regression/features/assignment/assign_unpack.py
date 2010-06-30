# singleton tuple <- singleton tuple
x, = 0,
print(x)
# singleton tuple <- singleton list
x, = [-1]
print(x)
# binary tuple <- binary tuple
x,y = 1,2
print(x,y)
# binary tuple swap
x,y = y,x
print(x,y)
# ternary tuple <- ternary tuple
x,y,z = 3,4,5
print(x,y,z)
# singleton list <- singleton list
[x] = [42]
print(x)
# singleton list <- singleton tuple
[x] = 43,
print(x)
# binary list <- binary list
[x,y] = [6,7]
# binary list <- binary tuple
[x,y] = [44,45]
print(x,y)
# binary tuple (parens) <- binary list
(x,y) = [7,8]
print(x,y)
# binary tuple <- result of function call
(x,y) = (lambda: (9,10))()
print(x,y)
# nested binary tuple (parens) <- nested binary tuple (parens)
((x,y),z) = ((11,12),13)
print(x,y,z)
# nested binary tuple <- nested binary tuple
(x,y),z = (14,15),16
print(x,y,z)

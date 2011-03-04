# test construction of booleans by calling the bool class

# call with zero arguments

print(bool())

# call with bool arguments

print(bool(True))
print(bool(False))

# call with int arguments

print(bool(1))
print(bool(2))
print(bool(-1))
print(bool(-2))
print(bool(0))

# call with float arguments

print(bool(1.0))
print(bool(2.0))
print(bool(-1.0))
print(bool(-2.0))
print(bool(0.0))

# call with complex arguments

print(bool(1j))
print(bool(-1j))
print(bool(0j))

# call with tuple arguments

print(bool(()))     # 0-tuple
print(bool((1,)))
print(bool(((),()))) # 2-tuple

# call with list arguments

print(bool([]))
print(bool([1]))
print(bool([1,2]))

# call with string arguments

print(bool(""))
print(bool("s"))
print(bool("st"))

# call with dict arguments

print(bool({}))
print(bool({1:1}))
print(bool({1:1, 2:2}))

# call with set arguments

#print(bool(set()))
#print(bool({1}))
#print(bool({1,2}))

# call with an object argument

print(bool(object()))

# call with a user defined class and object

class Foo(object): pass

print(bool(Foo))
print(bool(Foo()))

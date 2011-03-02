# should really have Bool cases in here too

# int `op` float, float `op` int

print(1 - 2.0)
print(1.0 - 2)

print(2 + 3.0)
print(2.0 + 3)

print(4 * 5.0)
print(4.0 * 5)

print(6 / 3.0)
print(6.0 / 3)

# int `op` complex, complex `op` int

print(1 - 2j)
print(1j - 2)

print(2 + 3j)
print(2j + 3)

print(4 * 5j)
print(4j * 5)

print(6 / 3j)
print(6j / 3)

# float `op` complex, complex `op` float

print(1.0 - 2j)
print(1j - 2.0)

print(2.0 + 3j)
print(2j + 3.0)

print(4.0 * 5j)
print(4j * 5.0)

print(6.0 / 3j)
print(6j / 3.0)

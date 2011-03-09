list = [1,2,3]

# list
print([x for x in list if x < 1])
print([x for x in list if x > 3])
print([x for x in list if x == 2])
print([x for x in list if x == x])
print([x for x in list if True])
print([x for x in list if False])

# set: this depends on the order that items in sets are printed.
print({x for x in list if x < 1})
print({x for x in list if x > 3})
print({x for x in list if x == 2})
print({x for x in list if x == x})
print({x for x in list if True})
print({x for x in list if False})

# dict: this depends on the order that items in dicts are printed.
print({x:x for x in list if x < 1})
print({x:x for x in list if x > 3})
print({x:x for x in list if x == 2})
print({x:x for x in list if x == x})
print({x:x for x in list if True})
print({x:x for x in list if False})

def printGen(gen):
   for item in gen:
      print(item)

# dict: this depends on the order that items in dicts are printed.
printGen((x for x in list if x < 1))
printGen((x for x in list if x > 3))
printGen((x for x in list if x == 2))
printGen((x for x in list if x == x))
printGen((x for x in list if True))
printGen((x for x in list if False))

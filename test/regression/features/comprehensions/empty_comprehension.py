# empty list
print([x for x in []])

# empty set
print({x for x in []})

# empty dict
print({x:x for x in []})

# empty gen
gen = (x for x in [])
for item in gen:
   print(item)

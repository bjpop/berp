# list
print([x for x in [1]])
# set
print({x for x in [1]})
# dict
print({x:x for x in [1]})
# gen
for item in (x for x in [1]):
   print(item)

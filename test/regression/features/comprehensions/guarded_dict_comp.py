# need a way to give canonical printout
# regular print may not be suited
print({x:1 for x in [1,2,3] if x < 1})
print({x:1 for x in [1,2,3] if x > 3})
print({x:1 for x in [1,2,3] if x == 2})
print({x:1 for x in [1,2,3] if x == x})
print({x:1 for x in [1,2,3] if True})
print({x:1 for x in [1,2,3] if False})

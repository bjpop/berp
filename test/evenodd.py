def even(x):
   if x == 0: return True
   return odd(x-1)

def odd(x):
   if x == 0: return False 
   if x == 1: return True
   return even(x-1)

print(even(22))
print(odd(22))

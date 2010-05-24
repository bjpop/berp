def even(x):
    if x == 0:
       return True
    else:
       return odd(x-1)

def odd(x):
    if x == 0:
       return False 
    else:
       return even(x-1)

count = 0
while count < 10:
   print(count)
   if even(count):
      print("even")
   if odd(count):
      print("odd")
   count = count + 1

x = 0
while(x < 10):
   try:
      print("in try")
      raise 12
   except:
      print("in except")
   finally:
      print("in finally")
      x = x + 1
      continue
      print("after continue")
print("after loop")

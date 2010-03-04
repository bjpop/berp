while(True):
   try:
      print("in try")
      raise 12
   except:
      print("in except")
      break
   finally:
      print("in finally")
print("after loop")

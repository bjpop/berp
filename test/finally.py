def f():
   count=0
   while count < 10:
      print(count)
      try:
         print ([1,2,3][3]) 
      except IndexError:
         print ("caught")
         return 5 
      finally:
         print ("finally")
         return 12 
   
      count += 1

print(f())

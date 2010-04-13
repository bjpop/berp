def f():
   yield 1
   yield False
   yield "hello" 

for x in f():
   print(x) 

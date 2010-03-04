# inefficient Fibonacci numbers 

def fib(n):
   if n <= 1: return 1
   return fib (n-1) + fib (n-2)

count = 30 
while count > 0:
   print(count)
   print (fib(count))
   count = count - 1

def f(n):
   nonlocal x
   x = 5
   def g(n):
      y = 42
      nonlocal x
      def h(m):
         print(x)
      h(4)
   g(3)

f(1) 

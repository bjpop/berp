def foo(foo, i) :
   if 0 < i :
      foo(foo, i-1)
   else:
      pass

foo(1,1)

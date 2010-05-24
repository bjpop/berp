def count(m,n):
   if m >= n:
      return m
   else:
      return count(m+1,n) 

print(count(0,1000000))

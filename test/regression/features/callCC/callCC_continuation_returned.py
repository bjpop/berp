print("start")
count = 0 
k = callCC(lambda x: x)
print(count)
if count < 10:
   count = count + 1
   k(k)
print("end")

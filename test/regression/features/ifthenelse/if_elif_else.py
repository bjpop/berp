def f(x,y):
    if x:
       return(1)
    elif y:
       return(2) 
    else:
       return(3)
print(f(True,True))
print(f(True,False))
print(f(False,True))
print(f(False,False))

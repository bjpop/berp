def verse(n):
   return ( str(n) + " bottles of beer on the wall,\n" +
            str(n) + " bottles of beer,\n" +
            "if take one down and pass it around,\n"
            "there'll be " + str(n-1) + " bottles of beer on the wall")

def song(n):
    while (n > 0):
      print(verse(n))
      n = n - 1
    print("0 bottles of beer on the wall")

def str(x): return x.__str__()

song(99) 

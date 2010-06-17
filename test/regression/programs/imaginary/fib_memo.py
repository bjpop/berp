# memoised fib

cache = {}

def fib(n):
    try:
       result = cache[n]
    except:
       if n <= 1: result = 1
       else:
          result = fib(n-1) + fib(n-2)
    cache[n] = result
    return result

print(fib(300))

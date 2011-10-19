# check if the identity of two objects is the same
def id_same(obj1, obj2):
    return id(obj1) == id(obj2)

class C(object): pass

# these should be the same (return True)
print(id_same(0,0))
print(id_same(True, True))
print(id_same(False, False))
print(id_same(None, None))
print(id_same(1.1, 1.1))
print(id_same('foo', 'foo'))
print(id_same(3 + 1j, 3 + 1j)) #CPython makes this false
x = object()
print(id_same(x, x))
c = C()
print(id_same(c, c))
# the identity of the identity of the same object, should also be the same
print(id_same(id(True), id(True))) #CPython sometimes makes this false and sometimes true

# these should be different (return False)
print(id_same(0,1))
print(id_same(True, False))
print(id_same(None, 1))
print(id_same(1.1, 2.5))
print(id_same('foo', 'bar'))
print(id_same(3 + 1j, 4 + 1j))
print(id_same(object(), object()))
print(id_same(C(), C()))
# the identity of the identity of different object, should also be different
print(id_same(id(True), id(False)))
# the identity of the identity of an object should be different to the identity of the object
print(id_same(id(True), True))

# The "monotonicity" example in the PEP "The Python 2.3 Method Resolution Order"
# by "Michele Simionato". This one was used to demonstrate the the mro of
# Python 2.2 was not monotonic. But it should be error free.

# This one should be error free. 

class A(object): pass
class B(object): pass
class C(object): pass
class D(object): pass
class E(object): pass
class K1(A,B,C): pass
class K2(D,B,E): pass
class K3(D,A):   pass
class Z(K1,K2,K3): pass

# The second example in the PEP "The Python 2.3 Method Resolution Order"
# by "Michele Simionato".

# This one should be error free.

O = object
class F(O): pass
class E(O): pass
class D(O): pass
class C(D,F): pass
class B(D,E): pass
class A(B,C): pass

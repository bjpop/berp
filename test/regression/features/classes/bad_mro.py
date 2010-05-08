# The first example in the PEP "The Python 2.3 Method Resolution Order"
# by "Michele Simionato".

# This one should produce an error.

O = object
class X(O): pass
class Y(O): pass
class A(X,Y): pass
class B(Y,X): pass
class C(A,B): pass

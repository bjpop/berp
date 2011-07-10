# Copyright   : (c) 2010 Bernie Pope
# License     : BSD-style
# Maintainer  : florbitous@gmail.com 

# A convenience Makefile.
# Mostly a wrapper for cabal, except for the running of test cases.

.PHONY : all 
all:
	cabal build

.PHONY : clean 
clean:
	cabal clean

.PHONY : configure 
configure:
	cabal configure

.PHONY : install 
install:
	cabal install --disable-library-profiling --disable-executable-profiling --disable-documentation

.PHONY : test
test:
	shelltest -j2 --execdir --with='berp' test/regression

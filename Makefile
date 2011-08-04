# Copyright   : (c) 2011 Bernie Pope
# License     : BSD-style
# Maintainer  : florbitous@gmail.com

# A convenience Makefile.

.PHONY: all configure clean install
all configure clean install:
	$(MAKE) $@ --directory=libs
	$(MAKE) $@ --directory=compiler
	$(MAKE) $@ --directory=interpreter

.PHONY : test
test:
	shelltest --color --execdir --with='berp --clobber' test/regression -- -j1

Berp, an implementation of Python 3
-----------------------------------

License and Copyright
---------------------

Berp is distributed as open source software under the terms of the BSD
License (see the file LICENSE in the top directory).

Author: Bernie Pope, copyright 2009, 2010, 2011.

Contact information
-------------------

Email Bernie Pope:

   florbitous <at> gmail <dot> com

Building and installing
-----------------------

Berp uses the cabal infrastructure for configuring, building and installation.
However, for convenience a Makefile is provided to orchestrate the process.

To build and install, use:

   make install

To clean, use:

   make clean

Testing
-------

Berp uses shelltestrunner for regression testing. Tests can be run like so:

   make test

Shelltestrunner can be installed from Hackage:

   cabal install shelltestrunner

Don't worry if some tests fail.

Directory structure
-------------------

---- libs                                # translator, and runtime libraries
     |
     |---- src
           |
           |---- include                 # C header files
           |
           |---- Berp
                 |
                 |---- Base              # runtime primitives
                 |     |
                 |     |---- Builtins    # implementation of Python's builtins
                 |     |
                 |     |---- StdTypes    # standard Python classes
                 |
                 |---- Compile           # translator from Python to Haskell


---- compiler                            # front end of the compiler
     |
     |---- src


---- interpreter                         # front end of the interactive interpreter
     |
     |---- src


---- test
     |
     |---- regression                    # regression tests
           |
           |---- features                # tests for specific language features
           |
           |---- programs                # Python programs

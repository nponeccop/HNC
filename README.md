# Quick Start

## Windows

Install Haskell Platform then run:
```
cabal update
cabal install --only-dependencies
configure
build
dist\build\spl-hnc\spl-hnc hn_tests\euler6.hn
```

## *nix

Install GHC or Haskell Platform and run:

```
cabal update
cabal install happy
PATH=$PATH:~/.cabal/bin
cabal install --only-dependencies

cabal configure
cabal build
./dist/build/spl-hnc/spl-hnc ./hn_tests/euler6.hn
```

# Advanced use

- read wiki
- run our main test suite (`dist/build/spl-test-hunit-exe/spl-test-hunit-exe`)
- run `hnc` with either `-O` or `--dump-opt` option to see our first attempts at optimization
- follow instructions below to feed generated `.cpp` files to a C++ compiler 
  and linker either directly or by using our extension to Boost.Build.

You'll need MSVC/GCC, Boost and Boost.

# Under the hood

HNC is an open-source cross-platform compiler based on modern technologies: Glasgow Haskell Platform, 
UUAGC attribute grammar preprocessor, Parsec parsing library, HOOPL graph optimization library. 
The codebase is tiny: less than 4 KLOC, in the spirit of VPRI Ometa.

## State of affairs

Many HN programs can already be compiled into an ugly functional subset of C++ and 
then into executables and run (see `hn_tests` folder for `.hn` sources and `.cpp` targets). 
A UDP echo server and a few Project Euler problems are the only useful programs so far, 
but mostly because we are too lazy to write more examples.

## What is done

- Parser
- Type inference using UUAG, including injection of explicit template parameters when C++ doesn't infer them
- Identifier- and scope-preserving translation from AST into graph IR and back using HOOPL dominator analysis
- A rudimentary and buggy optimizer of the IR using HOOPL
- Compiler of closures into C++ functors using UUAG (almost)
- C++ pretty printer (almost)
- A Boost.Build plugin to integrate .hn files into C++ projects

## What is not done

- Proper error reporting
- Loops and assignments
- The inliner is buggy for HOFs
- The instantiator of polymorphic code into monomorphic is not implemented
- Module system, namespace support, HNI implementation and C++ integration in general are rudimentary or missing
- Priorities of C++ infix operators are broken
- RAII should be used with care
- Our Boost-based implementation of a generalization of <code>std::bind1st</code> only works under MSVC
- SPL support is almost missing
- Polymorphic constants like “empty list” are not supported

# Setting up MSVC and Boost

- Install free Visual C++ Express or non-free Visual Studio
- Download Boost library from boost.org. 
- Extract `boost` subfolder from the distribution. It's the folder containing header files for all libraries.

Create `config.cmd` one level above HNC folder, so it's not under source control:

```cmd
@set INCLUDE=folder-containing-boost-subfolder
@call "%VS100COMNTOOLS%..\..\VC\vcvarsall.bat"
```

`VS100COMNTOOLS` is environment variable name set by VC 10.0 installer. Use `set | findstr COMN` to find 
out which version(s) of Visual C you have installed and change the `.cmd` file accordingly.

run `testAll.cmd` from `hn_tests` folder. You should see `tmp-*.cpp` files being generated 
from `.hn` sources and compiled into `.obj` files.

# Setting up GCC

The generated code is not specific to MSVC or Windows, so any other Boost-compatible C++ compiler 
and platform should work too. However, the scripts to run test suite are not there yet. 

To run `deref1.cpp` test manually with GCC, run

```
gcc -c -I../cpplib/include deref1.cpp
```

from `hn_tests` folder if Boost headers are installed globally to `/usr/include`, or

```
gcc -c -I../cpplib/include -Ifolder-containing-boost-subfolder deref1.cpp
```

if Boost headers are manually unpacked locally.

## License

Distributed under GNU Lesser General Public Licence Version 3.


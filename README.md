# Quick Start

[![Build Status](https://secure.travis-ci.org/nponeccop/HNC.png?branch=master)](http://travis-ci.org/nponeccop/HNC)

## Compilation

Install Haskell then run:

```
cabal update
cabal sandbox init
cabal install
.cabal-sandbox/bin/spl-hnc -O hn_tests/euler6.hn
```

## Arch Linux

Add 2 ArchHaskell binary repositories to `pacman.conf`. Install
some prebuilt packages from there to avoid building them. GHC comes
as a dependency.

```
sudo pacman -S cabal-install haskell-{adjunctions,haskell-src-exts,hunit,logict,parsec,quickcheck,safe}
```

Proceed to the general compilation steps described above

## Advanced use

- read wiki
- run our main test suite (`dist/build/spl-test-hunit-exe/spl-test-hunit-exe`)
- run `hnc` with either `-O` or `--dump-opt` option to see our first attempts at optimization
- follow instructions below to feed generated `.cpp` files to a C++ compiler 
  and linker either directly or by using our extension to Boost.Build.

You'll need MSVC/GCC, Boost and Boost.Build.

# Under the hood

HNC is an open-source cross-platform compiler based on modern technologies: Glasgow Haskell Platform, 
UUAGC attribute grammar preprocessor, Parsec parsing library, HOOPL graph optimization library, unification-fd structural unification library. The codebase is tiny: less than 4 KLOC, in the spirit of VPRI Ometa.

## State of affairs

Many HN programs can already be compiled into an ugly functional subset of C++ and 
then into executables and run (see `hn_tests` folder for `.hn` sources and `.cpp` targets). 
A UDP echo server and a few Project Euler problems are the only useful programs so far, 
but mostly because we are too lazy to write more examples.

## What is done

- Parser
- Type inference using UUAG, including injection of explicit template parameters when C++ doesn't infer them
- Identifier- and scope-preserving translation from AST into graph IR and back using HOOPL dominator analysis
- An optimizer of the IR using HOOPL (almost; only inlining and dead code elimination)
- Compiler of closures into C++ functors using UUAG (almost)
- C++ pretty printer (almost)
- A Boost.Build plugin to integrate .hn files into C++ projects

## Recent advances as of Jul 21 2015

- Fixed almost all optimizer bugs
- Working on loop generation

## What is not done

- Proper error reporting
- Assignments
- The instantiator of polymorphic code into monomorphic is not implemented
- Module system, namespace support, HNI implementation and C++ integration in general are rudimentary or missing
- Priorities of C++ infix operators are broken
- RAII should be used with care
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
gcc -I../cpplib/include test.cpp ../cpplib/lib.cpp -lstdc++ --std=c++0x
```

from `hn_tests` folder if Boost headers are installed globally to `/usr/include`, or

```
gcc -I../cpplib/include -Ifolder-containing-boost-subfolder test.cpp  ../cpplib/lib.cpp -lstdc++ --std=c++0x
```

if Boost headers are manually unpacked locally.

## License

Distributed under GNU Lesser General Public Licence Version 3.


#!/usr/bin/sed -f
s|( .*, dist/build/.*\.o )||g;
s|( .*, .stack-work/.*\.o )||g;
s|Compiling ||g

call "%VS80COMNTOOLS%..\..\VC\vcvarsall.bat"
cl -c -Id:\home2\libsrc\boost_1_37_0 -I..\..\cpplib\include  -EHa %1
call "%VS80COMNTOOLS%..\..\VC\vcvarsall.bat"
cl -Id:\home2\libsrc\boost_1_37_0 -I../cpplib/include -Ox -Zi -EHs -GL %1 -MT ../cpplib/lib.cpp wsock32.lib /link /release /opt:ref,icf /ltcg
@echo off
echo %1
if not exist %1.hn goto quit
call xtest.cmd %1 || pause "Failure!!"
rem d:\home\progs\svn\svn revert %1.cpp && 
rem d:\home\progs\svn\svn move %1.cpp failing\  && 
rem d:\home\progs\svn\svn move %1.hn failing\
:quit
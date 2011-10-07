@echo off
@call ..\..\..\config.cmd
cd ..
rem @%CONFIG%
rem @%BUILD% && echo Running!  
 
%BD%\build\spl-hnc\spl-hnc.exe hn_tests\%1 %2
cd hn_tests

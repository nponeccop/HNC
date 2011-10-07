@echo off
@call ..\..\..\..\config.cmd
rem @%CONFIG%
rem @%BUILD% && echo Running!    
cd ..\..
%BD%\build\spl-hnc\spl-hnc.exe hn_tests\failing\%1 %2
cd hn_tests\failing
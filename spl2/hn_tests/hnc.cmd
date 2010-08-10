@echo off
@call ..\..\..\config.cmd
cd ..
rem @%CONFIG%
rem @%BUILD% && echo Running!  
cd hn_tests 
%BD%\build\spl-hnc\spl-hnc.exe %1 %2


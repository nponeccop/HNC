@echo off
@call ..\..\..\..\config.cmd
rem @%CONFIG%
rem @%BUILD% && echo Running!    
%BD%\build\spl-hnc\spl-hnc.exe %1 %2
@echo off
@call ..\..\..\config.cmd
cd ..
rem @%CONFIG%
@%BUILD% && echo Running!  
cd cpplib 


@echo off
rem @call ..\..\config.cmd
cd ..
dist\build\spl-hnc\spl-hnc.exe -O hn_tests\%1 %2
cd hn_tests

@echo off
rem @call ..\..\config.cmd
cd ..\..\..
dist\build\spl-hnc\spl-hnc.exe --dump-opt hn_tests\opt\!\%1 %2
cd hn_tests\opt\!\

@echo off
if not exist %1.hn goto quit
call hnc %1.hn > %1.cpp && call cpp2 %1.cpp && pause "Success!!"
:quit
if not exist %1.hn goto quit
call hnc %1.hn > %1.cpp && call cpp %1.cpp && d:\home\progs\svn\svn move %1.cpp .. && d:\home\progs\svn\svn move %1.hn ..  :quit
@echo off
path %APPDATA%\cabal\bin;%PATH%
cabal build

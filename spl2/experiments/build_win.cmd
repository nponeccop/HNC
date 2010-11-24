for /f "tokens=4" %%G in ('findstr Revision Spli.hs') do set R=%%G

mkdir build_tmp
ghc.exe -outputdir tmp\build_win -o spli.exe --make Spli.hs
echo %R%


for /f "tokens=4" %%G in ('findstr Revision Spli.hs') do set R=%%G

mkdir build_tmp
ghc.exe -outputdir build_tmp -o spli_r%R%.exe --make Spli.hs


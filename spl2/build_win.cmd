for /f "tokens=4" %%G in ('findstr Revision Spli.hs') do set R=%%G

gch --make Spli.hs spli_r%R%.exe 


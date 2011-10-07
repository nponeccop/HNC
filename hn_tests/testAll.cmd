call "%VS80COMNTOOLS%..\..\VC\vcvarsall.bat"
for %%a in (*.hn) do xtest2.cmd %%~na
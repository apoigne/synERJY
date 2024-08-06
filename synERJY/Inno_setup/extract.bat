@echo off
cd %~dp0
echo.

echo Extracting sE package
extract -o sEpack.zip -d "."
echo.
echo Removing temporary files
del "sEpack.zip"
del "extract.exe"
del "extract.bat"

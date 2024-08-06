@echo off
echo seset

set SE_HOME=C:\Dokumente und Einstellungen\Axel\workspace\synERJY

if .%1. == .. goto _vsnet
if .%1. == .n. goto _vsnet
if .%1. == .m. goto _msys
rem if .%1. == .v. goto _vs6
if .%1. == .a. goto _avr
if .%1. == .-r. goto _reset

:_vs8
echo accessing Visual Studio .NET
set OSTYPE=win32
echo MSVCDir=%MSVCDir%
call  "C:\Programme\Micros~1.NET\Common8\Tools\vsvars32.bat"
echo copy %SE_HOME%\.serc_win32 %SE_HOME%\.serc
copy %SE_HOME%\.serc_msys %SE_HOME%\.serc
goto :_end_target

:_vs7
echo accessing Visual Studio .NET
set OSTYPE=win32
echo MSVCDir=%MSVCDir%
call  "C:\Programme\Micros~1.NET\Common7\Tools\vsvars32.bat"
echo copy %SE_HOME%\.serc_win32 %SE_HOME%\.serc
copy %SE_HOME%\.serc_msys %SE_HOME%\.serc
goto :_end_target

:_msys
set OSTYPE=msys
echo copy %SE_HOME%\.serc_msys %SE_HOME%\.serc
copy %SE_HOME%\.serc_msys %SE_HOME%\.serc
goto :_end_target

:_vs6
echo accessing Visual Studio V6
set OSTYPE=win32
echo MSVCDir=%MSVCDir%
call "C:\Programme\Microsoft Visual Studio\VC98\Bin\vcvars32.bat"
echo copy %SE_HOME%\.serc_win32 %SE_HOME%\.serc
copy %SE_HOME%\.serc_win32 %SE_HOME%\.serc
goto :_end_target

:_avr
rem set OSTYPE=win32
set AVRGCC=C:/WinAVR/avr
goto :_end_target

:_reset
set SE_HOME=
set OSTYPE=
set AVRGCC=
set MSVCDir=
echo del %SE_HOME%\.serc
del %SE_HOME%\.serc
goto :_end_target

:_end_target

echo OSTYPE  = %OSTYPE%
echo SE_HOME = %SE_HOME%

cd %SE_HOME%

doskey se.=cd %SE_HOME%
doskey se.s=cd %SE_HOME%\src
doskey se.t=cd %SE_HOME%\target
doskey se.t1=cd %SE_HOME%\target\AT90s8515
doskey se.tm=cd %SE_HOME%\target\ATmega162
doskey se.avr=cd C:\sylla\se\L\atmel\AvrGccTest
doskey se.e=cd C:\sylla\se\L\examples\
doskey se.m=cd %SE_HOME%\target\msys
doskey se.d=cd %SE_HOME%\target\dsk6713

doskey nmake=nmake -f Makefile.win32 $*

doskey ..=cd ..
doskey ...=cd ..\..
doskey ....=cd ..\..\..

doskey ~=cd \sylla

:_ende

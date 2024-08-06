echo off
if ".%VS80COMNTOOLS%." == ".." goto no_VS8
call  "%VS80COMNTOOLS%\vsvars32.bat"
goto vs_end

:no_VS8
if ".%VS71COMNTOOLS%." == ".." goto no_VS7
call  "%VS71COMNTOOLS%\vsvars32.bat"
goto vs_end

:no_VS7
echo cannot locate Visual Studio .NET

if not exist "%ProgramFiles%\Microsoft Visual Studio\VC98\Bin\vcvars32.bat" goto no_VS6

call  "%ProgramFiles%\Microsoft Visual Studio\VC98\Bin\vcvars32.bat"
goto vs_end

:vs_end

if not exist "%SE_HOME%\target\dsk6713" goto ccs_end
call "%SE_HOME%\target\dsk6713\DosRun.bat"

:ccs_end

:sEstart
if ".%SE_HOME%." == ".." goto no_sE

"%SE_HOME%\bin\synERJY.exe"
goto end

:no_VS6
echo cannot locate neither VisualStudio .NET nor VisualStudio Version 6
pause
goto end

:no_sE
echo cannot start synERJY since the environment variable SE_HOME is missing.
echo if synERJY is installed for the first time the system must be restarted
echo to activate the respective entry in the registry.
pause
goto end

:end

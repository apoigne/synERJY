@echo off
rem pad  path definition

if .%1. == .. goto pad_base
goto pad_command

:pad_base
if NOT  "%PAD_BASE%" == "" goto pad_info
set PAD_BASE=%PATH%
:pad_info
path
if "%PAD_BASE%" == "%PATH%" goto pad_isbase
echo is extended to PAD_BASE
echo BASE=%PAD_BASE%
goto pad_ende

:pad_isbase
echo is PAD_BASE
goto pad_ende

:pad_command
echo %0 %1

if .%1. == .-h. goto pad_help
if .%1. == .-r. goto pad_reset
if .%1. == ... goto pad_print
if .%1. == .se. goto pad_sehome
if .%1. == .a. goto pad_winavr
if .%1. == .au. goto pad_avrutil
if .%1. == .m. goto pad_mingw
echo unknown command: %1
echo Usage:
:pad_help
echo %0 -r # reset
echo %0 se # include synERJY/bin
echo %0 a  # include WinAVR/bin
echo %0 au # include WinAVR/utils/bin
echo %0 m  # include MinGW/bin
goto pad_until

:pad_reset
path=%PAD_BASE%
goto pad_until

:pad_sehome
path=%SE_HOME%\bin;%PATH%
goto pad_until

:pad_se
path=C:\sylla\se\sE\bin;%PATH%
goto pad_until

:pad_winavr
path=C:\WinAVR\bin;%PATH%
goto pad_until

:pad_avrutil
path=C:\WinAVR\utils\bin;%PATH%
goto pad_until

:pad_mingw
path=C:\msys\1.0\bin;C:\MinGW\bin;%PATH%
goto pad_until

:pad_print
sh -c  "IFS=: ; for F in $PATH; do echo $F; done"
goto pad_until

:pad_until
shift
if NOT .%1. == .. goto pad_command
goto pad_ende

:pad_ende

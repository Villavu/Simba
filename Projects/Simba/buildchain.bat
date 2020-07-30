@echo off

set lazbuild=%1
set buildmode=%2

%lazbuild% --build-mode=%buildmode% ..\SimbaScript\SimbaScript.lpi || exit 1
%lazbuild% ..\SimbaResources\SimbaResources.lpi || exit 1

..\SimbaResources\SimbaResources.exe %3 %4 %5 %6 %7 %8 %9 || exit 1
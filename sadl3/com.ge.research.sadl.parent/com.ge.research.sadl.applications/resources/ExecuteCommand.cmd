::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
:: This batch file is the main entry point into SADL command-line interface.
::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
@echo off
setlocal
set WORKSPACE=..\workspace
cd /d %~dp0
%~dp0\eclipsec.exe -console -noSplash -data %WORKSPACE% -application com.ge.research.sadl.applications.ExecuteCommand %*

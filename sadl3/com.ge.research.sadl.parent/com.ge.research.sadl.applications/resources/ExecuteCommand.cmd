::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
:: This batch file is the main entry point into SADL command-line interface.
::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
@echo off
cd /d %~dp0

setlocal
set LOG4J_CONFIGURATION_FILE=log4j2.properties

%~dp0\eclipsec.exe -application com.ge.research.sadl.applications.ExecuteCommand -consolelog -data ..\workspace -noSplash %*

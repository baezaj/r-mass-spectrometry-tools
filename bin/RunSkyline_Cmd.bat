@echo off

SETLOCAL EnableDelayedExpansion


:: Extracting the arguments
FOR /f %%i IN ("%1%") DO (
	SET filedrive=%%~di
	SET filepath=%%~pi
	SET filename=%%~ni
	SET fileextension=%%~xi
)

:: Directories used
:: SET ROOT_ANALYSIS_DIR=D:\Josue_Baeza_JSB54327\SystemCheck_testing
SET WORKING_DIR=D:\Josue_Baeza_JSB54327\SystemCheck_testing\NIST_peptides_2mm-column

:: Raw file name
SET RawFile="%filedrive%%filepath%%filename%%fileextension%"

:: Skyline files used
SET SkylineCmd="C:\Program Files\Skyline\SkylineCmd.exe"
SET SkylineFile="D:\Josue_Baeza_JSB54327\SystemCheck_testing\NIST_peptides_2mm-column\NIST_peptides_2mm-column.sky"
SET SkylineTemplate="%WORKING_DIR%\Spotfire Export Template 10Jun2024.skyr"
SET SkylineOutputFile=Output.sky
SET SkydFile = %SkylineOutputFile%d
SET SkylineReport=%RawFile%


REM Lets get rid of previously used .skyd files
IF EXIST %WORKING_DIR%/%SkydFile% DEL /f /q %WORKING_DIR%/%SkydFile%


REM Import Skyline template and save in new dir
%SkylineCmd% --in="%SkylineFile%" --out="%WORKING_DIR%\%SkylineOutputFile%" --overwrite 


ECHO Root Analysis Dir: %ROOT_ANALYSIS_DIR%
ECHO Working Analy Dir: %WORKING_DIR%


REM Open the new Skyline file, import raw file and export report
%SkylineCmd% --in="%WORKING_DIR%\%SkylineOutputFile%" --import-file="%RawFile%" --save --report-add=%SkylineTemplate% --report-conflict-resolution=overwrite --report-name="Spotfire Export" --report-file="%RawFile%.csv" --report-format=csv --report-invariant



ENDLOCAL

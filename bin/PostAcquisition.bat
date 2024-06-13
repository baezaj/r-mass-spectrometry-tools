@echo OFF

:: This is the batch script that will run after every MS run. 
:: Include this script to the "Post Acquisition Program" in the Xcalibur pane.
:: Every MS Run will 

SETLOCAL EnableDelayedExpansion


REM XCalibur will pass the file path (absolute) as one argument (aka %1%). This section will parse the path into different variables. 

:: Extracting the arguments
FOR /f %%i IN ("%1%") DO (
	SET filedrive=%%~di
	SET filepath=%%~pi
	SET filename=%%~ni
	SET fileextension=%%~xi
)

:: Raw file directory (absolute path)
SET RawFile="%filedrive%%filepath%%filename%%fileextension%"

:: ThermoRawFileParser application
SET ThermoRawFileParser="C:\Proteomics\ThermoRawFileParser\v1.4.3\ThermoRawFileParser.exe"

:: Output directory where metadata will be saved.
SET OutputDir=C:\Path\To\Metadata



REM Now let's run ThermoRawFileParser program on the raw file to get metadata

:: Running program
"%ThermoRawFileParser%" -i="%RawFile%" -f=4 -m=1









SET MyFile=%filedrive%%filepath%%filename%-metadata.txt
SET Keyword=Device acquisition method
:: List of System Check methods. If a new one is created, it must be added here. separate method names with a space " "
SET SystemCheckMethod=Systemsuit_2mm_spiderman_UV 



:: Loop through each line of the input file
FOR /f "tokens=1,2 delims==" %%A IN (%MyFile%) DO (
	SET Field=%%A
	SET Entry=%%B

	:: Search for keyword
	IF "!Field!"=="%Keyword%" (
	SET MethodDir=!Entry!
	)
)

ECHO RawFile: %RawFile%
ECHO MetadataFile: "%MyFile%" 
ECHO MethodDir: "%MethodDir%"

:: Extract the method file name and save as variable
FOR %%A IN ("%MethodDir%") DO (
	SET MethodName=%%~nA
	ECHO Method Name: !MethodName!
)


:: Check if MethodName is not in list
:: If MethodName IS NOT an ApprovedMethod, script stops.
:: If MethodName IS an ApprovedMethod, script continues to next section
FOR %%A IN (%SystemCheckMethod%) DO (
	IF NOT "%%A" == "%MethodName%" (
		ECHO Sorry, try again. Variable is not in the list
		GOTO End
	)
)


REM If the MethodName is part of the SystemCheckMethod list, then proceed to the Skyline processing

ECHO Yes. Variable is in the list! Now lets process the file with Skyline.

:: SkylineCmd batch script
"D:\Josue_Baeza_JSB54327\Scripts\RunSkyline_SystemCheck.bat" %RawFile%


:End


ENDLOCAL

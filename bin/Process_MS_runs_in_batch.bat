@echo off

SETLOCAL EnableDelayedExpansion

FOR %%A IN (%~1/*.raw) DO (

	echo Processing file: %%A
	
	call "C:\Path\to\PostAcquisition_script.bat" %%A
)

ENDLOCAL

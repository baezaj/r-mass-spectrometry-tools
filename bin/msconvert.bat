@echo off

cd C:\Users\Path\to\your\data\

"C:\Users\linds\AppData\Local\Apps\ProteoWizard 3.0.20037.71f8ae6b3 64-bit\msconvert.exe" -v --zlib --64 --mzML --filter "peakPicking true 1-" --filter "demultiplex optimization=overlap_only" *.raw

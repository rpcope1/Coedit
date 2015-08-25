Coedit setup program
===

This coedit project creates the coedit setup program.
The project contains 3 configurations. Each one takes the content of one of the nux32/nux64/win32 folders
and put it in the output program at compile time (eg .data section). 

Raw Zip
===

The shell scripts take the content of the nux32/nux64/win32 folders to make some simple zip files.
They are proposed alternatively to the setup program. The scripts are launched automatically when
the setup program is about to be compiled (as pre-build process).

Todo by hand for each release
===

- change the setup program _outputFilename_ for each configuration as well as the text printed to the console, according to the new version.
- change the text in the _version.txt_ file.
- put the content (programs, icon, license, etc.) in each of nux32/nux64/win32 folders.
- compile on each platform with the right project configuration.
- zip the setup programs. (in the future: as a post-build process ?).

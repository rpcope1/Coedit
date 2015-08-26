This folder contains the files necessary to make a Coedit release.
The process is semi-automated and achieved by compiling a project in Coedit itself.

Coedit setup program
===

This coedit project (_cesetup.coedit_) creates the coedit setup program.
The project contains 4 configurations:

- _devel-win32_: it does not takes part in the release process. 
- win32, nux32, nux64: they take respectively the content of their matching sub-folder and put it in the output program at compile time (using the `import(file)` expression).
The output program is then compressed by the post-build process, using the scripts named `setupzip-<arch>`.

Raw Zip
===

The shell scripts named `zip-<arch>` take the content of their matching sub-folder to make an archive.
They are proposed alternatively to the setup program. The scripts are launched automatically when the setup program is about to be compiled (as pre-build process).

Todo by hand for each release
===

- change the setup program _outputFilename_ for each configuration as well as the text printed to the console, according to the new version.
- change the text in the _version.txt_ file.
- put the content (programs, icon, license, etc.) in each of the nux32/nux64/win32 folders.
- compile on each platform with the right project configuration.
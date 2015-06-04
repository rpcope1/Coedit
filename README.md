
***Compile*** & ***Edit*** in _D_![](https://github.com/BBasile/Coedit/raw/master/logo/coedit.png)

Coedit is an IDE for the [D2](http://dlang.org) _DMD_ compiler.

![](https://github.com/BBasile/Coedit/raw/master/lazproj/coedit.win7.33.png)

**Features**
--------
- multi platform (_Windows_, _Linux_).
- strictly based on *DMD* (the alternative compilers are not supported).
- comprehensive project format and two advanced project editors (one for the options and configurations, another for the files).
- compile and run directly from the UI.
- single click to compile and execute an unsaved module (aka a _runnable module_).
- single click to compile and _unittest_ a module. (even a phobos one).
- synchronized edition in a block.
- D2 syntax highlighter, folds, regions, identifier markup.
- symbol list of the module being edited.
- static libraries manager.
- search and replace.
- _todo comments_ analyzer.
- user-defined tools powered by a string interpolation system (in a single click: submit the current module to _dfmt_ or _dscanner_ !)
- full [D Completion Daemon](https://github.com/Hackerpilot/DCD) integration (completion, hints, call tips, jump to symbol declaration).
- mini file browser.

**Missing things before the first version**
---------------------------------------
This is the last beta, all the features are implemented.
Until version 1, the quality will be improved and any new bug fixed.

**Project information**
-------------------
- status:  version 1 beta 3.
- _ETA_: mid June, ideally the 10.
- license: MIT.
- programmed in Object Pascal with [Lazarus & FPC](http://www.lazarus.freepascal.org) as IDE & compiler.

**Setup or build**
--------------
Windows and Linux binaries are available for each [release](https://github.com/BBasile/Coedit/releases).
The latest Coedit development version must be build from the sources.

The procedure is described in the [**first section of the wiki**](https://github.com/BBasile/Coedit/wiki#detailed-setup-procedure)

**GUI preview**
-----------
Windows version (Windows 7, x86):
![Win screen-cap](https://github.com/BBasile/Coedit/raw/master/lazproj/coedit.win7.png "Coedit GUI preview")

Linux version (OpenSuse 13.2, Kde, x86_64):
![Nux screen-cap](https://github.com/BBasile/Coedit/raw/master/lazproj/coedit.linux.kde.png "Coedit GUI preview")
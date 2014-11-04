Coedit
======

Coedit is a simple IDE for the [D2](http://dlang.org) lang. (**Co** mpile & **Edit**).

Current features
----------------
- multi platform (Win/Linux).
- full featured project format and advanced project editor.
- compile and run directly from the UI.
- instant run (without saving, script-like).
- synchronized edition in a block.
- D2 syntax highlighter, folding, identifier markup.
- module symbol list.
- static libraries manager.
- search and replace.
- user-defined tools powered by a string interpolation system.
- [D Completion Daemon](https://github.com/Hackerpilot/DCD) integration for completion proposal and source code hints.
- mini file browser.

Missing features before the first beta
--------------------------------------
- Options editor. (the big missing thing)

Project information
-------------------
- status: alpha 7.
- license: MIT.
- programmed in Object Pascal with [Lazarus & FPC](http://www.lazarus.freepascal.org) as IDE & compiler.
- based on *DMD* (the alternative backends, LDC or GDC, are not supported).

Setup & test
------------
The latest tagged Coedit versions are available as some pre-build binaries for Win and Nux.
The latest Coedit development version must be build from the sources.

The complete procedure is described in the [**first section of the wiki**](https://github.com/BBasile/Coedit/wiki#detailed-setup-procedure)

GUI preview
-----------
Windows version (Windows 7, x86):
![Win screen-cap](lazproj/Gui.tease.png "Coedit GUI preview")

Linux version (OpenSuse 13.1, Kde, x86_64):
![Nux screen-cap](lazproj/Gui.tease.kde.png "Coedit GUI preview")
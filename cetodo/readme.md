ceTodo
======

Tool designed to analyze the _TODO comments_ in D source files.
It's written in D using Coedit. 

To build it, either [libdparse](https://github.com/Hackerpilot/libdparse)
must be setup in the [libman](https://github.com/BBasile/Coedit/wiki#library-manager-widget) 
as described in this [tutorial](https://github.com/BBasile/Coedit/wiki#lets-build-a-static-library),
or *libdparse* submodule must be cloned with Coedit repository (`git submodule init` or `update`).

- `cetodo_submodule.coedit`: coedit project based on *libdparse* as a submodule.
- `cetodo_libman.coedit`: coedit project based on *libdparse* as a *libman* entry.

This tool is mandatory to enable the *todo list widget*.
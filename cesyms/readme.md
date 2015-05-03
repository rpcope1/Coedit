ceSyms
======

Tool designed to build a symbol tree for a given D module.
It's written in D using Coedit.

To build it, either [libdparse](https://github.com/Hackerpilot/libdparse)
must be setup in the [libman](https://github.com/BBasile/Coedit/wiki#library-manager-widget) 
as described in this [tutorial](https://github.com/BBasile/Coedit/wiki#lets-build-a-static-library),
or *libdparse* submodule must be cloned with Coedit repository (`git submodule init` or `update`).

Notice that *libdparse* can be easily build in Coedit using the [metad project](https://github.com/BBasile/metad).

- `cesyms_submodule.coedit`: coedit project based on *libdparse* as a submodule.
- `cesyms_libman.coedit`: coedit project based on *libdparse* as a *libman* entry.

This tool is mandatory to enable the _symbol list widget_.
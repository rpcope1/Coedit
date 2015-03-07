ceSyms
======

Tool designed to build a symbol tree for a particular D module.
It's written in D using Coedit. To build it, [libdparse](https://github.com/Hackerpilot/libdparse)
must be setup in the [libman](https://github.com/BBasile/Coedit/wiki#library-manager-widget) 
as described in this [tutorial](https://github.com/BBasile/Coedit/wiki#lets-build-a-static-library).

This tool is mandatory to enable the new _symbol list widget_ (formerly called _static explorer_) 
It replaces DMD JSON output to list the declarations within a module.
ceSyms
======

Tool designed to build a symbol tree for a particular D module.
It's written in D using Coedit. To build it, [libdparse](https://github.com/Hackerpilot/libdparse)
must be setup in the [libman](https://github.com/BBasile/Coedit/wiki#library-manager-widget) 
as described in this [tutorial](https://github.com/BBasile/Coedit/wiki#lets-build-a-static-library).

This tool is mandatory to enable the new _Symbol list widget_. 
If missing, the old _static explorer widget_ still does the same but it's from far less
efficient since it actually compiles while only the AST is needed.

The new tool does not take the module correctness in account.
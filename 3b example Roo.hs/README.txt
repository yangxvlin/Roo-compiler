In today's Compiler Clinic, I promised to show an example of a main Roo.hs modulePreview the document, which imports the compiler modules and, depending on command-line options, performs the particular task (parse, pretty-print, do semantic analysis, or compile).  It assumes that

    Roo -a <file>  will just do the parsing

    Roo -p <file>  will produce a pretty-printed version of <file>

    Roo -n <file>  will do semantic analysis, but no code generation

    Roo <file>  will do the full compilation and output an Oz target program

The imports suggest one way of splitting the compiler into modules, and in fact, there may be other modules (they just are not imported here, but they may be used by some of the listed modules).  For example, a module SymbolTable makes sense, which offers functions that can build and use local symbol tables.  There could be a module specifically for managing type information - whatever seems to be helpful in managing the complexity of the overall task.

Some modules make use of "Either" types to produce results of the form "Right result" or "Left error_msg", to distinguish results of successful processing from errors.  (The type "Result", with data constructors "Okay" and "Err" is just a home-grown variant of "Either".)
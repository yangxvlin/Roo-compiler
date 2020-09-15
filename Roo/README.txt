
With this README file you should also have found the following files:

Makefile:
    A rudimentary makefile for the COMP90045 project 2020.

JoeyScanner.x
    An alex specification for a subset, Joey, of our source language.
    Running alex with this specification as input will produce a
    scanner, called JoeyScanner, for Joey. It is quite possible that 
    you will *not* need this file as a starting point for your Roo 
    parser; it depends on which other tools you plan to use. The 
    main purpose of including the alex specification here is to be 
    precise about lexical assumptions that carry over to Roo, such 
    as the definition of a valid identifier.

RooParser.hs
    A parser for Roo written using the Parsec parser combinator library.
    It depends on RooAST.hs. It does not depend on RooScanner.x or 
    RooScanner.hs, because Parsec also offers support for scanning. 
    Again, you can use this as a starting point for your Roo parser,
    or you may choose to do things differently. 

RooAST.hs
    Data structures to represent abstract syntax trees (ASTs) for Roo.
    You may use this as a starting point for your design of similar
    data structures for Roo. Quite possibly you will not just need
    to extend the given definitions, you may have to alter them or
    redesign parts to suit the complexities of Roo.

PrettyRoo.hs
    A pretty-printer (well, not really - just a stub).

Roo.hs
    The main program. 

hello.roo, io.roo, asg.roo:
    Three example Roo programs. The roo suffix is for "Roo" - since
    Roo is a subset of Roo, the examples are also valid Roo programs.

To get started, study the files, compile and execute them. On a
Unix machine you should be able to just type 

    make

and that should generate the executable, called Roo, for you.
To build a scanner, type

    make RooScanner

To run these on some source programs, do

    RooScanner < io.roo
or
    Roo -a io.roo

(the parser is written so that it understands command-line arguments;
the -a option causes an AST to be output; the -p option is for
pretty-printing.)

If your Unix system doesn't seem to recognise 'RooScanner' and 
'Roo', that could be because your Unix PATH variable hasn't been
set correctly. For now, just try instead

    ./RooScanner < io.roo
and
    ./Roo -a io.roo


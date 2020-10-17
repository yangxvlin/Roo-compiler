This directory contains the C source files for an Oz emulator.
To generate the emulator, simply type

  make

This will generate an executable called oz which takes the name
of an Oz file as its command line argument.

The generation has been tested in September 2020 on the MSE server

  nutmeg2.eng.unimelb.edu.au

Run your Roo compiler to compile a Roo program, and then feed that output into the Oz interpretter.

$ ./Roo prog.roo > prog.oz
$ ./oz prog.oz

Alternatively, write prog.oz by hand and run.
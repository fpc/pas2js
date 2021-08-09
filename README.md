Welcome to pas2js
=================

Pas2js is an open source Pascal to JavaScript transpiler.
It parses Object Pascal and emits JavaScript.

Official site:
http://wiki.freepascal.org/pas2js

********************************************************************************

Directories
===========

packages - source files needed to compile pas2js programs
demo - examples
bin - output folder for binaries, e.g. pas2js.exe
compiler - source files to compile the compiler
units - output folder of compiler ppu/o files
utils - utilities

********************************************************************************

Building
========

1. Install the Free Pascal compiler (fpc), 3.0.4 or better, 32 or 64bit.
Either from their website
https://www.freepascal.org/download.html
Or via Lazarus
http://wiki.freepascal.org/Getting_Lazarus
Or on MacOS via homebrew
Or on Linux via your package manager.

2. building pas2js

2.1 Under Linux/macOS type
make all

This creates with a 64-bit fpc the executable "bin/x86_64-linux/pas2js"
and a basic config file "bin/x86_64-linux/pas2js.cfg".


2.2 Under Windows type

Make sure that you use the make.exe from fpc, not from Delphi by setting
the "PATH":
For example if you installed the 32-bit version of fpc in C:\YourPathOfFPC\3.0.4

set PATH=C:\YourPathOfFPC\3.0.4\bin\i386-win32;%PATH%

If you installed the 64-bit version of fpc in C:\YourPathOfFPC\3.0.4 use

set PATH=C:\YourPathOfFPC\3.0.4\bin\x86-64-win64;%PATH%

Then compile
make all

If you see "Error makefile ... Command syntax error" your "set PATH" was
not correct.

When "make all" finished it created with a 32-bit fpc the executable
"bin/i386-win32/pas2js.exe" and a basic config file "bin/i386-win32/pas2js.cfg".


********************************************************************************

Configuration
=============

pas2js requires unit search paths (-Fu) in order to find its rtl units and
rtl.js. Building and installing should have created a default pas2js.cfg
containing those parameters.

http://wiki.freepascal.org/pas2js.cfg


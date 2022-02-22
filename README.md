## Welcome to pas2js

Pas2js is an open source Pascal to JavaScript transpiler.
It parses Object Pascal and emits JavaScript.

Official site:
[http://wiki.freepascal.org/pas2js](http://wiki.freepascal.org/pas2js)


## Directories

* **packages** - source files needed to compile pas2js programs
* **demo** - examples
* **bin** - output folder for binaries, e.g. pas2js.exe
* **compiler** - source files to compile the compiler. This is an external submodule link.
* **units** - output folder of compiler ppu/o files
* **utils** - utilities

## Building

### Compiler
You need to install the Free Pascal compiler (fpc), 3.2.0 or better, 32 or 64bit.
Either from their website
```text
https://www.freepascal.org/download.html
```
Or via Lazarus
```text
http://wiki.freepascal.org/Getting_Lazarus
```

Or on MacOS via homebrew. On Linux this can be done via your package manager
as well if it has the latest version.

### building pas2js

### Complete checkout

The makefile expects the sources of FPC to be present under the `compiler` directory.
You can copy/clone the FPC sources there, or make a symlink to the FPC sources.

The git repository contains a submodule link to the FPC sources.
You can use this to have an automatically correct version of fpc. This is useful for the `fixes` and `release` branches, as they automatically get the tested combination of pas2js and fpc sources.

When you do a `git clone`, make sure you also specify the `--recurse-submodules`  option:
```sh
git clone --recurse-submodules https://gitlab.com/freepascal.org/fpc/pas2js.git
```

If you didn't do this, issue the init and update commands:
```sh
git submodule update --init --recursive
```
This will have the same effect as when you do a --recurse-submodules.

After doing a `git pull` or a `git switch`, you must also update the submodule:
```sh
git pull
git submodule update --init --recursive
```
This will pull any upstream changes to your local FPC repository.

#### TortoiseGit ####

To update the submodule 'compiler' do the following:
In the Git Synchronization dialog, after the "Pull", click on the "Submodule Update" button.

#### Under Linux/macOS
type the following command:
```sh
make all
```

This creates with a 64-bit fpc the executable `bin/x86_64-linux/pas2js`
and a basic config file `bin/x86_64-linux/pas2js.cfg`.


#### Under Windows


Make sure that you use the `make.exe` from fpc, not the one from Delphi by setting the
`PATH`: For example if you installed the 32-bit version of fpc in
`C:\YourPathOfFPC\3.2.0`:
```bat
set PATH=C:\YourPathOfFPC\3.0.4\bin\i386-win32;%PATH%
```
If you installed the 64-bit version of fpc in `C:\YourPathOfFPC\3.2.0` use
```bat
set PATH=C:\YourPathOfFPC\3.0.4\bin\x86-64-win64;%PATH%
```
Then compile
make all

If you see "Error makefile ... Command syntax error" your "set PATH" was
not correct.

When `make all` has succesfully finished it created with a 32-bit fpc the executable
`bin/i386-win32/pas2js.exe` and a basic config file
`bin/i386-win32/pas2js.cfg`.


## Configuration

pas2js requires unit search paths (-Fu) in order to find its rtl units and
rtl.js. Building and installing should have created a default pas2js.cfg
containing those parameters.

[http://wiki.freepascal.org/pas2js.cfg]([http://wiki.freepascal.org/pas2js.cfg])



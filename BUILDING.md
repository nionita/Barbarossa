Building Barbarossa
===================

This is a quick guide to build Barbarossa.

Prerequisites
-------------

Barbarossa can be built on every operating system for which following tools are available:
- the GHC compiler (version >= 7.10.2)
- the stack tool (version >= 1.1.2)

The current version uses the libraries from LTS 3.5 (https://www.stackage.org/lts-3.5),
which will be automatically downloaded when stack configures the project.

Building for first time
-----------------------

After downloading of the sources (or cloning the repository), run:

    stack setup

Stack will download the correct GHC and libraries (this will take a while if this
it is the first time using that versions).

After that you can build the program with:

    stack build

One of the last lines of the output tells you where the produced executable (with a name
like Barbarossa, for Linux, or Barbarossa.exe, for Windows) was copied.

You should copy that binary in a folder which is appropriate for your chess engines.

Building with SSE 4.2 support
-----------------------------

In order to build the binary with SSE 4.2 support (which uses the popcount machine
instruction instead of a library version), use:

    stack build --flag Barbarossa:sse42

The produced binary will be ~10% faster, but run only on (Intel/AMD) CPUs which
support that extension.

Building for a different architecture
-------------------------------------

If you are building the binary on a 64 bit machine, the default GHC will be a
64 bit one and the produced binary will be a 64 bit binary too. If you want to build
the binary for the 32 bit architecture you can run:

    stack setup --arch i386

followed by:

    stack build --arch i386

Those command will need a few minutes to setup the i386 environment (GHC & needed
32 bit libraries). stack uses different working directories for the new architecture,
so you still have the 64 bit binary as well.

Please be aware that GHC might not be able to compile a 64 bit binary if your machine
is 32 bit.

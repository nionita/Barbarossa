Building Barbarossa
===================

This is a quick guide to build Barbarossa.

Prerequisites
-------------

Barbarossa is built for 64 bit Windows and Linux on (Intel/AMD) CPUs with SSE 4.2 (the code is compiled
with `-msse4.2` and uses the popcount instruction); 32 bit and older CPUs are not supported.

Needed tools:
- the stack tool (version 3 or newer), which installs the right GHC itself

The current version uses GHC 9.10 and the libraries of Stackage LTS 24.12 (https://www.stackage.org/lts-24.12),
which stack downloads when it configures the project.

Building for the first time
---------------------------

After downloading of the sources (or cloning the repository), run:

    stack setup

Stack will download the correct GHC and libraries (this will take a while the first time).

Building the engine
-------------------

For playing, build the speed variant of the engine (without the payload of the training data collection):

    stack build --flag Barbarossa:-qscollect Barbarossa:exe:Barbarossa

or, with the same result, `bash scripts/build-prod.sh`.

One of the last lines of the output tells you where the produced executable (Barbarossa on Linux,
Barbarossa.exe on Windows) was copied. Copy it to a folder which is appropriate for your chess engines.

Development builds
------------------

A plain `stack build` builds all executables: the engine and the development tools (SelfPlay, TuneSGD,
LogScan, UCIScores), with the quiescence data collection included (flag `qscollect`, on by default).
Further build flavours (e.g. a deterministic SelfPlay for performance tests) are described in `AGENTS.md`.

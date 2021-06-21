# The SSM Build System

Compiling an SSM program roughly consists of two stages: the frontend, which
compiles the source representation (Haskell EDSL) to platform-agnostic C, and
the backend, which compiles C to platform-specific, executable machine code.
Since the stages are largely independent, they are each built upon a different
build system specialized for the job.

This document describes the architecture of each build system, and how they
interact with one another.

## Compiler Frontend

The frontend of the compiler is written in Haskell, and compiles an SSM
program's source represention to generated C code. This compiler component is
built using Haskell Stack.

Since source programs are themselves written in Haskell files, the compiler
frontend is exposed as a Haskell library rather than an executable. This library
includes some helper functions to help users build Haskell files containing SSM
programs into executables that output the compiled program. These helper
functions are defined in `edsl/Ssm/Compiler/Cli.hs`.

For example, the user may write their program in `MyProgram.hs`:

```haskell
import SSM
import Ssm.Compiler.Cli (compileCli)

myProgram :: SSM ()
myProgram = ...

main :: IO ()
main = compileCli myProgram
```

The definition of the `main` function allows `MyProgram.hs` to be built as an
executable that compiles `myProgram` to C, and writes it to a file. It can even
be invoked with Stack outside the ssm-edsl project directory:

```
stack --stack-yaml /path/to/ssm-edsl/stack.yaml runghc MyProgram.hs
```

By default, the output filepath is derived from the name of the entry point; in
this case, it would be `myProgram.c`. This can be overriden with the `-o` flag:

```
stack --stack-yaml /path/to/ssm-edsl/stack.yaml runghc MyProgram.hs -- -o myprog.c
```

## Compiler Backend

The compiler backend is built upon GNU Make, and the plumbing of its build
system is defined in a series of `Build.mk` files; the top-level `Build.mk`
file, located at the root of this repo, is the entry point for the build system.
More information about how the build system is organized and implemented can be
found in the documentation in there (i.e., useful hacking on or against it), but
some high-level details are described below.

### Build Directory

The build system is responsible for creating the build directory, and performing
compilation and linking tasks within the build directory. The build directory is
`build/<platform>`; all build artifacts are left behind here. The build system
defines three phony targets useful for managing the build directory:

- `make clean`: removes the build directory, i.e., `rm -rf build/<platform>`
- `make distclean`: removes all platforms' build directories, i.e., `rm -rf build`
- `make make_builddir`: creates the build directory without building anything,
  i.e., `mkdir build/<platform>`, and prints the build directory path (useful
  for scripting around).

### Platform and Runtime

The build system is also responsible for selecting the platform-specific
toolchain and compiler flags, and building and linking in the language runtime
library to the executable. The language runtime library consists of
platform-agnostic code, organized under the `runtime/` directory, and
platform-specific, under `platform/`.

The SSM "platform" designates the environment that a SSM program is compiled to
run on. It defaults to `simulation`, but can be explicitly specified by
explicitly defining the `PLATFORM` variable. For instance:

```
make PLATFORM=teensy40
```

The platform encompasses the architecture (e.g., Arm vs x86), the
hardware (e.g., STM32F4 vs nrf52) the system environment (e.g., Linux
userspacevs Zephyr RTOS), and the purpose (e.g., release vs debug vs testing).
Some platforms will implement the temporal aspects of an SSM program (i.e.,
the executable will blocks and sleep in wall clock time), while others
may execute in simulation (i.e., execute all events as soon as possible),
primarily for testing and debugging purposes.

Each platform's code is located in the `platform/<platform-name>` directory; for
example, the `teensy40` platform is under `platform/teensy40`. The platform
directory contains a `Build.mk` that defines the platform's build configuration,
and defines a platform library `libplatform.a` that is linked into the target
executable.

The platform-agnostic runtime defines the interfaces between the generated
C code, the event and continuation scheduler, and the platform library, and also
implements the scheduler. Its build configuration, `runtime/Build.mk` exposes
the runtime library target `libssm.a`, which is linked into the target
executable.

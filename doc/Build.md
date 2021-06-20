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

The compiler backend is built upon GNU Make. Given a platform and a target, this
build system is responsible for creating the build directory, specifying
the platform-specific toolchain and compiler flags, building the runtime
(platform-agnostic) and platform libraries, and all built object files into an
executable. It also provides convenience phony targets for cleaning up the build
directory and uploading the built executable to a connected board.

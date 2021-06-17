# Testing the SSM EDSL Compiler

Our testing stack relies on the following external tools:

- Stack, for managing test suites and providing the CLI for running tests.
- Hspec, for managing tests within each test suite and handling CLI arguments.
- Quickcheck, for generating arbitrary programs, asserting properties about
  them, and shrinking them to produce minimal failing test cases.

Internally, we also provide the following components to help testing:

- An event trace data type (Output), which is a list of events that take place
  during execution.
- A definitional interpreter which evaluates a (LowCore) program to produce
  a (potentially infinite) event trace.
- The `trace` "platform", which implements trace statements to print out lines
  of output that can be parsed into an event trace and compared against the
  interpreters output trace.
- An implementation of `arbitrary` and `shrink` for (LowCore) programs that
  allow them to be randomly generated and shrunk by QuickCheck.
- Wrappers around the Make-based build system to thread those actions into
  QuickCheck's `PropertyM` monad.

## Instructions

### Running Tests

To run all tests:

```
stack test
```

This will run all test suites. To run a specific test suite, e.g., only the
regression test suite (named `1-regression-low`), run:

```
stack test :1-regression-low
```

If any specific regression test fails, Hspec will you that it can be rerun using
the `--match` parameter. You can tell Stack to forward this argument to Hspec:

```
stack test :1-regression-low --test-arguments '--match "/Regression.ManyConts"'
```

The `arbitrary` test suite (named `2-arbitrary`) uses QuickCheck to generate
random programs that are compiled and executed. By default, it generates 100
random programs per property. This can be adjusted via parameter:

```
stack test --test-arguments '--qc-max-success 420'
```

Options written about here are given in long form for clarity; shorthand is also
available:

- `--test-arguments '<args>'` abbreviates to `--ta '<args>'`
- `--match` abbreviates to `-m`
- `--qc-max-success` abbreviates to `-a`

See [Passing options to Hspec][hspec-options] for more details about available
options.

[hspec-options]: https://hspec.github.io/options.html

### Investigating Failed Tests

When a test case fails, a report will be dumped into `trace-report/`, in
a directory beginning with `test-`. Named regression tests will be labeled with
the test name, while randomly generated tests will be affixed with a unique
timestamp (reflecting when the test was invoked).

The report directories will contain most relevant information about a test case,
such as the source program, its the emitted C source, the compiled binary
executable, and the execution output. These can be statically investigated,
i.e., without invoking the Haskell or C compilers. The report directory also
contains a spec file that can be copied back into the regression test suite.

In the `trace-report/` directory, there is also a `redo-test` script to help
rerun tests. It can be used to copy the emitted C source back into the `genc`
directory so that it can be recompiled with `make`, or load the generated spec
file back into the regression test suite (which is helpful for adding a randomly
generated example into the persistent regression suite). The specific usage
documentation can be found by invoking the script without arguments.

### Adding Tests

The regression test suite(s) use hspec-discover to automatically populate the
suite with regression tests found in the same directory. For instance, for the
`regression-low` suite, this is done using the GHC directive in the top-level
`Spec.hs`, which synthesizes a `main` function that runs all the unit tests:

```haskell
{-# OPTIONS_GHC -F -pgmF hspec-discover #-}
```
New regression tests should be added to the `Regression` subdirectory next to
that spec file, ending with `Spec.hs`. For example, a regression test located at
`:test/regression-low/Regression/FooSpec.hs`:

```haskell
module Regression.FooSpec where

import qualified Test.Hspec as H

spec :: H.Spec
spec = ...
```

hspec-discover will run whatever is in the module's `spec` definition.

### Adding Test Suites

To add a test suite, add to the `tests` section of `package.yaml`. Stack's test
suite features are sparsely documented, but are fairly self-explanatory, and
based on [Cabal's][cabal-test]. Some things to note:

- When `stack test` is invoked without arguments, all test suites are run
  alphabetically. Thus, the name of each test suite is prepended with a number
  to specify the order they should run (e.g., simpler tests first, so that
  errors are caught earlier).

- Each test suite needs to specify an entry point, in the `main` field
  (corresponding to Cabal's `main-is` field). The specified module is
  conventionally named `Spec.hs`, and should define a `main :: IO ()` function.

- Each test suite should specify a list of source directories in the
  `source-dirs` field. This should include `test/lib`, the root of this
  project's test helper library (see below), and the root of the test suite
  where `Spec.hs` is found.

- The dependencies for a test suite, specified by the `dependencies` field,
  should include the `ssm` library, which will transitively include all
  dependencies the main compiler pipeline has.

[cabal-test]: https://www.haskell.org/cabal/release/cabal-1.18.1.2/doc/users-guide/developing-packages.html#test-suites

## Common Test Helpers

There is a lot of shared code between the different test suites, which are
accumulated under `:test/lib/Test/Ssm`. These helpers wrap the Quickcheck
library to provide a common interface for different test suites that may test
different compiler inputs and configurations.

QuickCheck works by trying to find counterexamples to user-defined properties,
of type `Property`. To support imperative tests, it also exposes the `PropertyM`
monad transformer, which allows other monads (in particular, `IO`) to be lifted
to within the context of a Quickcheck `Property`. The helpers defined in the
test library thread imperative `IO` procedures through the `PropertyM` monad.

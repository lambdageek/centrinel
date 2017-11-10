# Centrinel #

Restrict some C structs to reside in specific regions of the heap.

## Motivation ##

When using C to implement a runtime for a programming language, we may want to
distinguish internal runtime data structures (allocated using `malloc`/`free`
or some kind of memory pool or perhaps `mmap`ed from a dumped image) from user
objects allocated in a GC managed heap.

In [Mono](http://www.mono-project.com/) the runtime represents managed objects that derive
from `System.Object` using a C struct `typedef struct _MonoObject { ... } MonoObject`.  Mono uses the convention that a class dervies from class `T` if its first member is of type T:

```c
struct _MonoString {
	MonoObject obj; /* System.String derives from System.Object */
	...
};
```

Mono is transitioning to a regime where pointers to managed memory `MonoObject
*ptr` must be accessed by the runtime internals indirectly via handles<sup id="ref1">[1](#f1)</sup>: `MonoObjectHandle h`

This package provides a library that analyzes C files to find places where raw pointers to managed memory are used.

## Quick start example ##

Here is how one might use Centrinel and
[centrinel-report](https://github.com/lambdageek/centrinel-report) check an
autotools project using [Bear](https://github.com/rizsotto/Bear) to intercept
the C compiler invocations:

```
# assuming you have GHC and cabal installed
$ pushd $CENTRINEL_DIR # go to the centrinel source directory
$ echo "Building Centrinel"
$ cabal sandbox init
$ cabal install happy
$ cabal install alex
$ cabal install --dependencies-only
$ cabal configure
$ cabal build
$ popd
$ echo Building project using "bear"
$ pushd $SRC_DIR
$ ./autoconf ...
$ bear make
# produces a compile_commands.json file
$ popd
$ cd $CENTRINEL_DIR
$ curl -o centrinel-report.tar.gz https://github.com/lambdageek/centrinel-report/releases/download/v0.3.1/centrinel-report.tar.gz
$ tar xvzf centrinel-report.tar.gz
# writes a skeleton HTML report to centrinel-report/
$ cabal run centrinel -- --project $SRC_DIR/compile_commands.json --exclude $SRC_DIR/thirdparty --format json --output centrinel-report/centrinel-report.json
# produces centrinel-report/centrinel-report.json
$ cd centrinel-report && python -m SimpleHTTPServer 8000
# go look at http://localhost:8000/
```


## Installation ##

This is an early development prototype, so the setup is a bit involved.

You will need a recent GHC (tested with GHC 8.2.x, GHC 8.0.x, and 7.10.x) and cabal.

Dependencies: This program uses the `language-c` library for C parsing - you
will need the `alex` lexer and `happy` parser installed (either systemwide or
in the sandbox for this repository).  The steps below setup a sandbox and
install the needed programs into it. (You don't need `alex` and `happy` to run
the analysis, just to build the analyzer binary.)

```bash
git clone https://github.com/lambdageek/centrinel.git
cd centrinel
cabal sandbox init
cabal install happy
cabal install alex
cabal install --dependencies-only
cabal configure
cabal build
cabal run centrinel -- --help
```

### The `include/centrinel.h` header ###

Centrinel relies on some `__attribute__((...))` attributes and some
preprocessor defines to tell it what to analyze.  As a convenience, when
Centrinel invokes the C preprocessor, it automatically includes the file
[`include/centrinel.h`](include/centrinel.h) before the user code.  If running
from a sandbox, as above, the `cabal run centrinel` command will ensure that
Centrinel is able to find this file. If instead you use `cabal install`, the
Centrinel header will be copied to the cabal data directory, and the
`centrinel` binary that is built will refer to that location even if you
subsequently move the binary.

This header defines `__CENTRINEL__` to `1` to indicate that centrinel is
running; `__CENTRINEL_MANAGED_ATTR` attribute and `__CENTRINEL_MANAGED_REGION`
attribute specifier that you can use to annotate your structs, as well as
macros to define away certain GCC primitives that are not understood by
`language-c`.

The header is automatically included by centrinel, you don't have to include it explicitly.

## Usage ##

You should now be able to play around with it using `cabal run centrinel --
[ARGS]` (or `cabal repl` if you want to run the various steps separately).
Additionally, there is a script `centrinelcc` which can be used as `CC` in
`make` invocations as a drop-in replacement for `gcc` or `clang`.

### Just run the binary on a C file ###

Usage: `centrinel [--output FILE] -- CFLAGS CFILE`

The program understands a modicum of `cc` command line options such as `-D` and
`-I` and `-U` and will pass others through to the preprocessor.  A few options
that don't make sense for preprocessing (such as `-c` or `-MT` are understood
and silently dropped).  By default analysis results are printed to stdout; to
direct them to a file, add a `--output FILE` option.

```
$ cabal run centrinel -- c-examples/attrib.c
Errors:
c-examples/attrib.c:28: (column 2) [WARNING]  >>> Region mismatch: Region 1 and Region 2
  Additional locations:
          c-examples/attrib.c:8: (column 2)
          c-examples/attrib.c:13: (column 1)
          c-examples/attrib.c:27: (column 1)

c-examples/attrib.c:35: (column 5) [ERROR]  >>> Naked pointer(s) to managed object(s) found in declaration
  Pointer to managed heap XP
          in 0th argument 'x' at c-examples/attrib.c:35: (column 13)
          in function declaration 'foo'

c-examples/attrib.c:37: (column 11) [ERROR]  >>> Naked pointer(s) to managed object(s) found in declaration
  Pointer to managed heap struct Y *
          in return type
          in function declaration 'bar'
  Pointer to managed heap XP
          in 1st argument 'x' at c-examples/attrib.c:37: (column 26)
          in function declaration 'bar'

c-examples/attrib.c:39: (column 5) [ERROR]  >>> Naked pointer(s) to managed object(s) found in declaration
  Pointer to managed heap XP
          in 0th argument 'x' at c-examples/attrib.c:33: (column 28)
          in type defined at c-examples/attrib.c:33: (column 1)
          at c-examples/attrib.c:39: (column 10)
          in 0th argument 'f' at c-examples/attrib.c:39: (column 19)
          in function declaration 'baz'

```

**Note**: if you need to pass an option that starts with a dash `-` and you're
using `cabal run`, you should write `--` twice: once for cabal and once for
centrinel: `cabal run -- -- -DFOO -I../.. c-examples/attrib.c`)

### Use a compilation database JSON file ###

Usage: `centrinel [--output FILE] --project compile_commands.json`

A [Clang compilation
database](http://clang.llvm.org/docs/JSONCompilationDatabase.html) is a JSON
file that specifies a set of compiler invocations.  Tools such as `cmake` or
[Bear](https://github.com/rizsotto/Bear) can be used to generate the
compilation database (usually called `compile_commands.json`).  Centrinel can
use the database with the `--project` option:

```
$ bear make -C some_project_directory # Note: doesn't work on OSX with System Integrity Protection
$ cabal run centrinel -- --project compile_commands.json
```

### Replace `CC` in a make ###

The file [`scripts/centrinelcc`](scripts/centrinelcc) can be used as the value of the `CC` variable for `make`.
It will first run the real compiler (specified by the `REAL_CC` environment variable, or `cc` if unset) and
if compilation succeeds, it will invoke `centrinel` (which must be on the path)

#### Setup ####

These steps are not automated yet

```
$ cabal install
$ cp dist/build/centrinel/centrinel [somewhere on your PATH]
$ cp scripts/centrinelcc [somewhere on your PATH]
```

#### Example ####

```
$ export REAL_CC=/path/to/your/cc # if unset, defaults to 'cc'
$ make CC=centrinelcc
```

## Specifying a different C preprocesor ##

By default centrinel will use the value of the `REAL_CC` environment variable
to run the preprocessor, or `cc` if unset.  Another way to specify the compiler
is using the `--use-cc CC` option which takes precedence over the environment
variable.

## Specifying a JSON output format ##

**Experimental**: with `--format json` Centrinel will write analysis results as
a JSON blob (whose format is quite in flux at the moment).  The blob may be
consumed with, for example
[centrinel-report](https://github.com/lambdageek/centrinel-report)

## Excluding directories from analysis ##

With `--exclude DIR` (which may be specified multiple times), any input files
that are in `DIR` or one of its subdirectories will not be analyzed.  (Works
both with `-- CFLAGS CFILE` mode and with `--project JSON_FILE`).

## What works ##

* Region annotations on struct definitions (specified by
  `__attribute__((__region(N)))` with integer *N*) will be unified with regions
  inferred for the first member of the struct (provided it's another struct
  type) and conflicts will be reported.

* Pointers to structs **in region `__CENTRINEL_MANAGED_REGION`** (defined as
  `__region(1)` in `include/centrinel.h`) will elicit an error if they occur
  anywhere in a function prototype (either a declaration or a definition).

* Checking of function bodies for use of `__CENTRINEL_MANAGED_REGION` pointers.

* A way of annotating blessed functions/structs that are allowed to manipulate
  pointers to the managed heap without an error using `__CENTRINEL_SUPPRESS_ATTR(1)` which may be applied to:
  * a label at the beginning of a compound statement:
	```c
		{
			l: __CENTRINEL_SUPPRESS_ATTR(1) ;
			/* your code that manipulated managed objecs */
		}
	```
  * on a function declaration, to suppress warnings about the function arguments and results:
    ```c
	int __CENTRINEL_SUPPRESS_ATTR(1) foo (...); /* no warnings about pointers in ... */
	```
  * Additionally `__CENTRINEL_SUPPRESS_ATTR(0)` may be used to turn checking
    back in within a suppressed context.  This is useful for, for example,
    macro expansion: you want the body of the macro to be trusted, so the
    pointer checking is suppressed, but the arguments are unsuppressed so that
    they are checked.

# Footnotes #

<b id="f1">1</b>: One immediate benefit is that if a garbage collection happens while native runtime code is running, we don't have to pin the referenced object, which may reduce fragmentation. [↩](#ref1)

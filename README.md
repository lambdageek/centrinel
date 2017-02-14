# HeapGuard #

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

## Installation ##

This is an early development prototype, so the setup is a bit involved.

You will need a recent GHC (tested with GHC 8.0.x, but 7.10.x will probably
work too, and possibly 7.8 with minor patching).

Dependencies: *next* branch of [my fork of language-c](https://github.com/lambdageek/language-c) ([upstream](https://github.com/visq/language-c))

```bash
git clone https://github.com/lambdageek/language-c.git -b next
git clone https://github.com/lambdageek/use-c.git
cd use-c
cabal sandbox init
cabal sandbox add-source ../language-c
cabal install happy
cabal install alex
cabal install --dependencies-only
cabal configure
cabal build
```

You should now be able to play around with it using `cabal repl`

## Usage ##

There is a barebones executable `heapguard` usage is `heapguard PATH-TO-C-FILE`:

```
$ cabal run heapguard -- c-examples/attrib.c
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

## What works ##

* Region annotations on struct definitions (specified by
  `__attribute__((__region(N)))` with integer *N*) will be unified with regions
  inferred for the first member of the struct (provided it's another struct
  type) and conflicts will be reported.

* Pointers to structs **in region 1** will elicit an error if they occur
  anywhere in a function prototype (either a declaration or a definition).

## What's planned ##

* Checking of function bodies for use of region 1 pointers.

* A way of annotating blessed functions/structs that are allowed to manipulate
  pointers to the managed heap without an error.


# Footnotes #

<b id="f1">1</b>: One immediate benefit is that if a garbage collection happens while native runtime code is running, we don't have to pin the referenced object, which may reduce fragmentation. [↩](#ref1)

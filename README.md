# Smos

A semantic tree-based editor to replace Emacs Org Mode

## A replacement for emacs org mode

Smos intends to deprecate Emacs org mode by being more robust, more
customisable, by providing a better and more tool-friendly file
format and by using a more sane configuration language.

## Completely customisable

Smos is first and foremost a library with which you can build your own version.
It is similar to XMonad in this respect.
See [the default configuration](https://github.com/NorfairKing/smos/blob/development/smos/src/Smos/Default.hs)
for an example of a configuration.

## Future-proof file format

The Smos file format (`.smos`) is completely future proof because it is just
a subset of YAML:

```
- entry:
    state: TODO
    header: Use Smos
  forest:
  - state: DONE
    header: Don't mess it up
  - state: TODO
    header: be smart about it
```

This format is very easy to work with programmatically, and convenience
functions in the `smos-data` library are provided to operate on Smos data.

## Installation

Clone the repository:

```
$ git clone https://github.com/NorfairKing/smos
```

Use [Stack](haskellstack.org) to install Smos with the default configuration:

```
$ stack install :smos
```

Should you wish to change the key controls, you can do this by making your own
little Haskell project (also with stack), define the configuration you want to
use and pass it to the [`smos`](https://github.com/NorfairKing/smos/blob/development/smos/src/Smos.hs#L29)
library function.

## Contributing

Smos uses [hpack](https://github.com/sol/hpack) in addition to `stack`. It
uses a [Zifter](https://github.com/NorfairKing/zifter) script to maintain
consistency, enforce code quality, and run tests.

There is a `stack.yaml` in the project root, then a `project.yaml` under
each project directory, e.g. `smos/project.yaml`. `hpack` generates `.cabal`
files from these `.yaml` files, so if you need to make changes to the project
configuration then make these changes in the appropriate `package.yaml`.

To run the Zifter script, execute the following from the root project directory:

```
$ ./zift.hs run
```

This will take a long time on the initial execution.

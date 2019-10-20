# Checktestdata

[![Build status](https://gitlab.com/DOMjudge/checktestdata/badges/antlr_rewrite/pipeline.svg)](https://gitlab.com/DOMjudge/checktestdata/commits/antlr_rewrite)
[![Coverage Status](https://img.shields.io/coveralls/DOMjudge/checktestdata.svg)](https://coveralls.io/r/DOMjudge/checktestdata)
[![Coverity Scan Status](https://img.shields.io/coverity/scan/4325.svg)](https://scan.coverity.com/projects/checktestdata)

Checktestdata is a tool to verify the syntactical integrity of test cases in
programming contests like the ACM ICPC.

It allows you to specify a simple grammar for your testdata input files,
according to which the testdata is checked. In [the examples directory](examples)
you find two sample scripts `hello.ctd` and `fltcmp.ctd` from the
[DOMjudge](https://www.domjudge.org/) sample problems *hello* and
*fltcmp*, and under [examples/nwerc2008](examples/nwerc2008) the
scripts used for the [NWERC 2008 problemset](http://2008.nwerc.eu/contest/problemset).

## Grammar specification

[Checktestdata language specification](doc/format-spec.md)

We also have a [Haskell implementation](haskell_edsl/) of the
checktestdata program, which supports this specification as well as a
Haskell embedded domain-specific language.

## Installation and use


Requirements:

 * A modern C++ compiler.
   (GNU g++ >= 7.0 is known to work)
 * Libboost (http://www.boost.org/)
 * The GNU GMP libraries (http://gmplib.org/)

Command line for installing the build dependencies on Debian or
Ubuntu:
```
apt-get install make g++ libboost-dev libgmp-dev
```
For Redhat-like distributions try:
```
yum install make g++ boost-devel gmp-devel
```

In order to compile checktestdata, you need to install
<a href="https://bazel.build">Bazel</a>.
We recommend to run Bazel via the wrapper
<a href="https://github.com/bazelbuild/bazelisk">bazelisk</a>. To install bazelisk, execute
```
go get github.com/bazelbuild/bazelisk
# Add bazelisk to the default path.
export PATH=$PATH:$(go env GOPATH)/bin
```

To compile checktestdata and to run a number of tests:
```
bazelisk test //:test
```

Note that a few of the tests are
non-deterministic and may sometimes return an error.
This is ok, but the command above should run successfully more than 90% of the
time.

The checktestdata binary is available in `bazel-bin/checktestdata`.

For usage instructions run:
```
checktestdata --help
```


## Library

The commandline program is built upon the separate library
`libchecktestdata.hpp` (see `checktestdata.cc` as an example for how to use this
library) that can be used to write the syntax checking part of special compare
scripts. It can easily handle the tedious task of verifying that a team's
submission output is syntactically valid, leaving just the task of semantic
validation to another program.

When you want to support *presentation error* as a verdict, also in variable
output problems, the option whitespace-ok can be useful. This allows any
non-empty sequence of whitespace (no newlines though) where the SPACE command
is used, as well as leading and trailing whitespace on lines (when using the
NEWLINE command). Please note that with this option enabled, whitespace
matching is greedy, so the script code

    INT(1,2) SPACE SPACE INT(1,2)

does not match `1__2` (where the two underscores represent spaces), because the
first `SPACE` command already matches both, so the second cannot match
anything.


## Copyright & Licencing

Checktestdata is Copyright &copy; 2008 - 2018 by the checktestdata developers and
all respective contributors. The current checktestdata developers are Jeroen
Bransen, Jaap Eldering, Jan Kuipers, and Tobias Werth; see the git commits for
a complete list of contributors.

Checktestdata, including its documentation, is free software; you can
redistribute it and/or modify it under the terms of the two-clause
BSD license. See the file [COPYING](COPYING).

The M4 autoconf macros are licenced under all-permissive and GPL3+
licences; see the respective files under m4/ for details.

## Contact

The developers can be reached through the mailinglist
`domjudge-devel@domjudge.org`. You need to be subscribed before
you can post. Information, subscription and archives are available at:
https://www.domjudge.org/mailman/listinfo/domjudge-devel

Some developers and users of checktestdata linger on the IRC channel
dedicated to DOMjudge on the Freenode network:
server `irc.freenode.net`, channel `#domjudge`

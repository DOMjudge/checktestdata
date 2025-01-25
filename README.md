# Checktestdata

[![Build Status](https://img.shields.io/travis/DOMjudge/checktestdata.svg)](https://travis-ci.org/DOMjudge/checktestdata)
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

 * A C++ compiler that supports C++11 including regex support.
   (GNU g++ >= 4.9 and LLVM clang++ >= 3.5 are known to work)
 * Libboost (http://www.boost.org/)
 * The GNU GMP libraries (http://gmplib.org/)
 * Automake (https://www.gnu.org/software/automake/)
 * flexc++/bisonc++ (optional)

If you don't have `flexc++` and/or `bisonc++` available, you may use the release
branch where we've pre-generated the scanner/parser files.

Command line for installing the build dependencies on Debian or
Ubuntu, when using the release branch:
```
apt-get install make g++ libboost-dev libgmp-dev autotools-dev automake
```
For Redhat-like distributions try:
```
yum install make g++ boost-devel gmp-devel automake
```

To compile checktestdata, run:
```
./bootstrap
make dist
make
```

Leave out the `make dist` step if you use the pre-generated scanner/parser
files on the release branch.

Finally, you can run
```
make check
```
to run a number of tests. Note that a few of the tests are
non-deterministic and may sometimes return an error. This is ok, but the
`make check` command should run successfully more than 90% of the time.

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

Checktestdata is Copyright &copy; 2008 - 2025 by the checktestdata developers and
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

DOMjudge has a Slack workspace where a number of developers and users
of DOMjudge & checktestdata linger. More information can be found at
https://www.domjudge.org/chat

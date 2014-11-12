Checktestdata
=============

Checktestdata is a tool to verify the syntactical integrity of test cases in
programming contests like the ACM ICPC.


## Documentation

You can find the checktestdata language specification on
http://www.domjudge.org/docs/judge-manual-6.html.

Requirements:

 * recent g++ (> 4.8)
 * boost
 * boost-regex
 * flexc++/bisonc++ (optional)

If you don't have `flexc++` and/or `bisonc++` available, you may use the release
branch where we've pre-generated the scanner/parser files.

To compile checktestdata, run:
```
./bootstrap
make dist
make
```

Leave out the `make dist` step if you use the pre-generated scanner/parser
files on the release branch.


## Copyright & Licencing

Checktestdata is Copyright (c) 2008 - 2014 by the checktestdata developers and
all respective contributors. The current checktestdata developers are Jeroen
Bransen, Jaap Eldering, Jan Kuipers, and Tobias Werth; see the git commits for
a complete list of contributors.

Checktestdata, including its documentation, is free software; you can
redistribute it and/or modify it under the terms of the GNU General
Public License as published by the Free Software Foundation; either
version 2, or (at your option) any later version. See the file
COPYING.

The M4 autoconf macros are licenced under all-permissive and GPL3+
licences; see the respective files under m4/ for details.

## Contact

The developers can be reached through the mailinglist
`domjudge-devel@lists.a-eskwadraat.nl`. You need to be subscribed before
you can post. Information, subscription and archives are available at:
https://lists.a-eskwadraat.nl/mailman/listinfo/domjudge-devel

Some developers and users of checktestdata linger on the IRC channel
dedicated to DOMjudge on the Freenode network:
server `irc.freenode.net`, channel `#domjudge`

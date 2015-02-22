Checktestdata language specification
====================================

This specification is dedicated to the public domain. Its authors waive all
rights to the work worldwide under copyright law, including all related and
neighboring rights, as specified in the
[Creative Commons Public Domain Dedication (CC0 1.0)](http://creativecommons.org/publicdomain/zero/1.0/).

Grammar and command syntax below. A valid checktestdata program consists of a
list of commands. All commands are uppercase, while variables are lowercase
with non-leading digits. Lines starting with `#` are comments and ignored.

The following grammar sub-elements are defined:

    integer  := 0|-?[1-9][0-9]*
    float    := -?[0-9]+(\.[0-9]+)?([eE][+-]?[0-9]+)?
    string   := ".*"
    varname  := [a-z][a-z0-9]*
    variable := <varname> | <varname> '[' <expr> [',' <expr> ...] ']'
    value    := <integer> | <float> | <string> | <variable>
    compare  := '<' | '>' | '<=' | '>=' | '==' | '!='
    logical  := '&&' | '||'
    expr     := <term> | <expr> [+-] <term>
    term     := <term> [*%/] <factor> | <factor>
    factor   := <value> | '-' <term> | '(' <expr> ')' | <factor> '^' <factor>
    test     := '!' <test> | <test> <logical> <test> | '(' <test> ')' |
                 <expr> <compare> <expr> | <testcommand>

That is, variables can take integer, floating point as well as string values.
No dynamic casting is performed, except that integers can be cast into floats.
Integers and floats of arbitrary size and precision are supported, as well as
the arithmetic operators `+-*%/^` with the usual rules of precedence. An
expression is integer if all its sub-expressions are integer. Integer division
is used on integers. The exponentiation operator `^` only allows non-negative
integer exponents that fit in an unsigned long. String-valued variables can
only be compared (lexicographically), no operators are supported.

Within a string, the backslash acts as escape character for the following
expressions:

 * `\[0-7]{1,3}` denotes an octal escape for a character
 * `\n`, `\t`, `\r`, `\b` denote linefeed, tab, carriage return and backspace
 * `\"` and `\\` denote `"` and `\`
 * an escaped newline is ignored (line continuation)

A backslash preceding any other character is treated as a literal backslash.

Tests can be built from comparison operators, the usual logical operators
`!` `&&` `||` (not, and, or) and a number of test commands that return a
boolean value. These are:

<dl>
<dt><tt>MATCH(&lt;string&gt; str)</tt></dt>

<dd>Returns whether the next character matches any of the characters
    in 'str'.</dd>

<dt><tt>ISEOF</tt></dt>

<dd>Returns whether end-of-file has been reached.</dd>

<dt><tt>UNIQUE(&lt;varname&gt; a [,&lt;varname&gt; b ...])</tt></dt>

<dd>Checks for uniqueness of tuples of values in the combined (array)
    variables a, b, ... That is, it is checked that firstly all
    arguments have precisely the same set of indices defined, and
    secondly that the tuples formed by evaluating (a,b,...) at these
    indices are unique. For example, if x,y are 1D arrays containing
    coordinates, then <tt>UNIQUE(x,y)</tt> checks that the points
    (x[i],y[i]) in the plane are unique.</dd>

<dt><tt>INARRAY(&lt;value&gt; val, &lt;varname&gt; var)</tt></dt>

<dd>Checks if val occurs in the array variable var.</dd>
</dl>

The following commands are available:

<dl>
<dt><tt>SPACE</tt> / <tt>NEWLINE</tt></dt>

<dd>No-argument commands matching a single space (0x20) or newline
    respectively.</dd>

<dt><tt>EOF</tt></dt>

<dd>Matches end-of-file. This is implicitly added at the end of each
    program and must match exactly: no extra data may be present.</dd>

<dt><tt>INT(&lt;expr&gt; min, &lt;expr&gt; max [, &lt;variable&gt; name])</tt></dt>

<dd>Match an arbitrary sized integer value in the interval [min,max]
    and optionally assign the value read to variable 'name'.</dd>

<dt><tt>FLOAT(&lt;expr&gt; min, &lt;expr&gt; max [, &lt;variable&gt; name [, option]])</tt></dt>

<dd>Match a floating point number in the range [min,max] and
    optionally assign the value read to the variable 'name'. When the
    option 'FIXED' or 'SCIENTIFIC' is set, only accept floating point
    numbers in fixed point or scientific notation, respectively.</dd>

<dt><tt>STRING(&lt;value&gt; str)</tt></dt>

<dd>Match the string (variable) 'str'.</dd>

<dt><tt>REGEX(&lt;string&gt; str [, &lt;variable&gt; name])</tt></dt>

<dd>Match the extended regular expression 'str'. Matching is performed
    greedily. Optionally assign the matched string to variable 'name'.</dd>

<dt><tt>ASSERT(&lt;test&gt; condition)</tt></dt>

<dd>Assert that 'condition' is true, fail otherwise.</dd>

<dt><tt>SET(&lt;variable&gt; name '=' &lt;expr&gt; value])</tt></dt>

<dd>Assign 'value' to variable 'name'.</dd>

<dt><tt>UNSET(&lt;varname&gt; a [,&lt;varname&gt; b ...])</tt></dt>

<dd>Unset all values for variables a, b, ... This includes all values
    for array indexed variables with these names. This command should
    typically be inserted at the end of a loop after using UNIQUE or
    INARRAY, to make sure that no old variables are present anymore
    during the next iteration.</dd>

<dt><tt>REP(&lt;expr&gt; count [,&lt;command&gt; separator]) [&lt;command&gt;...] END</tt></dt>
<dt><tt>REPI(&lt;variable&gt; i, &lt;expr&gt; count [,&lt;command&gt; separator]) [&lt;command&gt;...] END</tt></dt>

<dd>Repeat the commands between the 'REP() ... END' statements count
    times and optionally match 'separator' command (count-1) times in
    between. The value of count must fit in an unsigned 32 bit int.
    The second command 'REPI' does the same, but also stores the
    current iteration (counting from zero) in the variable 'i'.</dd>

<dt><tt>WHILE(&lt;test&gt; condition [,&lt;command&gt; separator]) [&lt;command&gt;...] END</tt></dt>
<dt><tt>WHILEI(&lt;variable&gt; i, &lt;test&gt; condition [,&lt;command&gt; separator]) [&lt;command&gt;...] END</tt></dt>

<dd>Repeat the commands as long as 'condition' is true. Optionally
    match 'separator' command between two consecutive iterations.
    The second command 'WHILEI' does the same, but also stores the
    current iteration (counting from zero) in the variable 'i'.</dd>

<dt><tt>IF(&lt;test&gt; cond) [&lt;command&gt; cmds1...] [ELSE [&lt;command&gt; cmds2...]] END</tt></dt>

<dd>Executes cmds1 if cond is true. Otherwise, executes cmds2 if the
    else statement is available.</dd>
</dl>

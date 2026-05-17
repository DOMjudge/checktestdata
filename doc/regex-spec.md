Checktestdata Regex specification
=================================

A **reg**ular **ex**pression (or regex) can be used to match strings.
Formally, it describes a set of strings and a string is matched if it is contained in the set.

Regular expressions can contain both literal and special characters.
Most literal characters, like `A`, `a`, or `0`, are the simplest regular expressions and they simply match themselves.
Additionally, more complex regular expressions can be expressed by concatenating simpler regular expressions.
If *A* and *B* are both regular expressions, then *AB* is also a regular expression.
In general, if a string *x* matches *A* and another string *y* matches *B*, then the string *xy* matches *AB*.

Besides the literal characters, there are also the following special characters: `'('`, `')'`, `'{'`, `'}'`, `'['`, `']'`, `'*'`, `'+'`, `'?'`, `'|'`, `'\'`, `'^'`, `'.'`, `'-'`.
Their meaning is as follows:

* `.`: this matches any character, including newlines. If you need to match anything except the newline character use `[^\n]` instead.
* `[]`: indicates a set of characters.
Inside a set definition:
    * Literal characters can be listed and all of them will be matched, i.e., `[abc]` will match `'a'`, `'b'` as well as `'c'` but not `'abc'`.
    * Ranges can be specified with `-`, for example `[a-z]` will match any lowercase ASCII letter and `[0-9]` will match any digit.
    If `-` is escaped (e.g. `[a\-z]`) or if the character preceding it belongs to another range (e.g. `[a-a-z]`), or if it is the first or last character (e.g. `[-a]` or `[a-]`), it will match a literal `'-'`.
    It is an error if the first character of the range has a higher code point than the last (e.g., `[z-a]`).
    * The complement of a character set is formed if the first character of the set is `^`.
    For example `[^a]` will match anything except `'a'`.
    If `^` is escaped (e.g. `[\^]`) or if it is not the first character (e.g. `[a^]`) it will match a literal `'^'`.
    * `\` can be used to escape a special characters.
    However, most special characters do not need to be escaped.
    Only `'['` and `']'` must be escaped and `'^'` or `'-'` might need to be escaped depending on the position.
    For example both `[\-]` and `[-]` will match a literal `'-'`.
    If `\` is not followed by a special characters it matches a literal `'\'`.
    * It is an error if the character set does not specify any characters (e.g. `[]` or `[^]`).
* `{m,n}`: causes the resulting regular expression to match from `m` to `n` repetitions of the preceding regular expression.
Matching is done greedily, i.e., as many repetitions as possible are matched.
Omitting *m* specifies a lower bound of zero, and omitting *n* specifies an infinite upper bound.
It is an error if *m* is larger than *n*.
Both *m* and *n* must be an integer without sign and without leading zeros.
It is an error if the preceding regular expression is empty or ends with another repetition (e.g. `{1,2}{1,2}`). If you want to do that use `()` (e.g. `({1,2}){1,2}`).
* `{m}`: is a shorthand for `{m,m}`.
It is an error to omit `m`.
* `*`: is a shorthand for `{0,}`.
* `+`: is a shorthand for `{1,}`.
* `?`: is a shorthand for `{0,1}`.
* `|`: can be used to form the union of two regular expressions.
If *A* and *B* are both regular expressions, then *A|B* is also a regular expression.
In general, if a string *x* matches *A* or it matches *B*, then it also matches *A|B*.
Matching is done in *leftmost-first* fashion.
This means that any match of *A* is preferred over all matches for *B*.
This means that the checktestdata command `REGEX("p|ps")` will only extract `p` even if the input is `ps`.
* `(...)`: if *A* is  a regular expression then *(A)* is also a regular expression.
* `\`: escapes the subsequent special character.
If `\` is not followed by a special character it will match a literal `\` (e.g. `\d` will match `'\d'`).
Note that checktestdata strings also use `\` to escape characters.
Therefore, `REGEX("\\*")` becomes the regular expression `\*` and matches a literal `'*'`, not a variable amount of `\`.

## Notes

The regular expression syntax and behaviour is carefully chosen to match a common subset of many modern regular expression definitions and implementations like Perl, Python, JavaScript, Ruby, PHP, Java, C++, Rust, Go, ...
Advanced features like quantifiers, groups, lookahead, lookbehind, etc. are not supported.
Shorthands like `\d` or `[:digit:]` are also not supported, use `[0-9]` instead.

> [!WARNING]
> Earlier versions of checktestdata used POSIX-like regular expressions with *leftmost-longest* matching and support for `[:digit:]`.
> This is no longer supported and matching is done *leftmost-first* instead.

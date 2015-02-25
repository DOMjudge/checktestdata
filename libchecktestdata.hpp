/*
 * Library to verify testdata or program output syntax.
 */

#ifndef LIBCHECKTESTDATA_HPP
#define LIBCHECKTESTDATA_HPP

#include <iostream>

namespace checktestdata {

const int exit_failure = 2;

const int opt_whitespace_ok = 1; // ignore additional whitespace
const int opt_quiet         = 2; // quiet execution: only return status
const int opt_debugging     = 4; // print additional debugging statements

void init_checktestdata(std::istream &progstream, int opt_mask = 0);
/* Initialize libchecktestdata by loading syntax from progstream and
 * setting options from opt_mask bitmask. This function must be called
 * before any other function.
 */

bool parse_preset_list(std::string str);
/* Parse list of variable value assignments, to be used as preset
 * values when calling gentestdata(). Syntax of the list must be:
 *
 *   <name>=<value>[,<name>=<value> ...]
 */

bool checksyntax(std::istream &datastream);
/* Check testdata input/output in datastream against specified syntax.
 * Returns 'true' if the syntax is completely valid.
 */

bool gentestdata(std::ostream &datastream);
/* Generate random testdata according to specified syntax.
 * Optionally, certain variable values can be preset by first calling
 * parse_preset_list().
 * Returns 'true' if testdata was successfully created.
 */

} // namespace checktestdata

#endif /* LIBCHECKTESTDATA_HPP */

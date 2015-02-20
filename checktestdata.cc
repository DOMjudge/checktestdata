/*
   Checktestdata -- syntactically check testdata according to a specified grammar.

   For detailed information, see libchecktestdata.

   It's licensed under the 2-clause BSD license, see the file COPYING.
*/

#include <cstdlib>
#include <iostream>
#include <fstream>
#include <getopt.h>

#include "libchecktestdata.hpp"

using namespace std;
using namespace checktestdata;

#define PROGRAM "checktestdata"
#define AUTHORS "Jan Kuipers, Jaap Eldering, Tobias Werth"
#define VERSION CTD_VERSION "/" REVISION

const int exit_testdata = 1;

const char *progname;

int show_help;
int show_version;

struct option const long_opts[] = {
	{"whitespace-ok", no_argument, NULL,         'w'},
	{"generate",no_argument,       NULL,         'g'},
	{"preset",  required_argument, NULL,         'p'},
	{"debug",   no_argument,       NULL,         'd'},
	{"quiet",   no_argument,       NULL,         'q'},
	{"help",    no_argument,       &show_help,    1 },
	{"version", no_argument,       &show_version, 1 },
	{ NULL,     0,                 NULL,          0 }
};

void version()
{
        printf("%s -- version %s, written by %s\n\n",PROGRAM,VERSION,AUTHORS);
        printf(
"Copyright (c) 2008 - 2015 by the checktestdata developers and all\n"
"respective contributors. All rights reserved.\n"
"%s comes with ABSOLUTELY NO WARRANTY and is provided \"as is\".\n"
"You are free to modify and redistribute this program under the conditions\n"
"of the 2-clause BSD licence, see the file COPYING for more details.\n",PROGRAM);
}

void usage()
{
        printf(
"Usage: %s [OPTION]... PROGRAM [TESTDATA]\n"
"Check (or generate) TESTDATA according to specification in PROGRAM file.\n"
"If TESTDATA is '-' or not specified, read from stdin (or write to stdout).\n"
"\n"
"  -w, --whitespace-ok  whitespace changes are accepted, including heading\n"
"                         and trailing whitespace, but not newlines;\n"
"                         be careful: extra whitespace matches greedily!\n"
"  -g, --generate       don't check but generate random testdata\n"
"  -p, --preset=<name>=<value>[,...]\n"
"                       preset variable(s) <name> when generating testdata;\n"
"                         this overrules anything in the program.\n"
"  -d, --debug          enable extra debugging output\n"
"  -q, --quiet          don't display testdata error messages: test exitcode\n"
"      --help           display this help and exit\n"
"      --version        output version and copyright information and exit\n"
"\n",progname);
}

int main(int argc, char **argv)
{
	int whitespace_ok;
	int generate;
	int debugging;
	int quiet;
	string presets;

	int opt;

	progname = argv[0];

	/* Parse command-line options */
	whitespace_ok = 0;
	generate = debugging = quiet = show_help = show_version = 0;
	opterr = 0;
	while ( (opt = getopt_long(argc,argv,"+wgp:dq",long_opts,(int *) 0))!=-1 ) {
		switch ( opt ) {
		case 0:   /* long-only option */
			break;
		case 'w':
			whitespace_ok = 1;
			break;
		case 'g':
			generate = 1;
			break;
		case 'p':
			presets = optarg;
			break;
		case 'd':
			debugging = 1;
			break;
		case 'q':
			quiet = 1;
			break;
		case ':': /* getopt error */
		case '?':
			printf("Error: unknown option or missing argument `%c'.\n",optopt);
			exit(exit_failure);
		default:
			printf("Error: getopt returned character code `%c' ??\n",(char)opt);
			exit(exit_failure);
		}
	}

	if ( show_help    ) { usage();   return 0; }
	if ( show_version ) { version(); return 0; }

	// Check for program file
	if ( argc<=optind ) {
		printf("Error: no PROGRAM file specified.\n");
		usage();
		return exit_failure;
	}

	char *progfile = argv[optind];
	ifstream prog(progfile);
	if ( prog.fail() ) {
		cerr << "Error opening '" << progfile << "'.\n";
		exit(exit_failure);
	}

	// Set options for checksyntax
	int options=0;
	if (whitespace_ok) options |= opt_whitespace_ok;
	if (debugging    ) options |= opt_debugging;
	if (quiet        ) options |= opt_quiet;

	// Check for testdata file and check syntax
	bool testdata_ok = 0;

	init_checktestdata(prog, options);

	// Parse presets after initialization to have debugging available
	if ( !parse_preset_list(presets) ) {
		printf("Error parsing preset variable list.\n");
		exit(exit_failure);
	}

	if ( argc<=optind+1 ) {
		if ( generate ) {
			gentestdata(cout);
		} else {
			testdata_ok = checksyntax(cin);
		}
	} else {
		if ( generate ) {
			char *datafile = argv[optind+1];
			ofstream fout(datafile);
			if ( fout.fail() ) {
				cerr << "Error opening '" << datafile << "'.\n";
				exit(exit_failure);
			}
			gentestdata(fout);
			fout.close();
		} else {
			char *datafile = argv[optind+1];
			ifstream fin(datafile);
			if ( fin.fail() ) {
				cerr << "Error opening '" << datafile << "'.\n";
				exit(exit_failure);
			}
			testdata_ok = checksyntax(fin);
			fin.close();
		}
	}

	prog.close();

	if ( !generate ) {
		if ( !testdata_ok ) {
			exit(exit_testdata);
		}

		if ( !quiet ) cout << "testdata ok!" << endl;
	}

	return 0;
}

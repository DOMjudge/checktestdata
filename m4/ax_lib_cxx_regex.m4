# AX_LIB_CXX_REGEX
# ================
# Simple no arguments test for presence of std::regex
AC_DEFUN([AX_LIB_CXX_REGEX],[
	AC_PREREQ([2.61])
	AC_LANG_ASSERT([C++])

	AC_MSG_CHECKING(for proper std::regex implementation)
	AC_RUN_IFELSE([AC_LANG_PROGRAM([[#include <regex>]],[[std::regex testregex("[0-9]"); std::regex_search("test123",testregex);]])],
		[AC_MSG_RESULT([yes])],[AC_MSG_ERROR([no functioning std::regex found])],
		[AC_MSG_RESULT([impossible]), AC_MSG_NOTICE([cannot check for proper std::regex during cross-compile])])
])

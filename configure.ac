# Process this file with autoconf to produce a configure script.
AC_INIT([checktestdata],m4_esyscmd([date '+%Y%m%d' | tr -d '\n']]),
	[domjudge-devel@domjudge.org])
AC_PREREQ([2.64])
AC_CONFIG_SRCDIR([checktestdata.cc])

# Set default {CXX,LD}FLAGS. This might screw up portability, but
# adds important security. Only set these flags when none are supplied
# by the user.
for flag in CXXFLAGS LDFLAGS ; do
	AC_MSG_CHECKING([whether configure should try to set $flag])
	if test x`eval echo '${'$flag'+set}'` = xset ; then res=no ; else res=yes ; fi
	eval enable_${flag}_setting=$res
	AC_MSG_RESULT($res)
done

DEF_CXXFLAGS="-g -O2"
DEF_LDFLAGS=""

AC_LANG_PUSH([C++])
AX_APPEND_COMPILE_FLAGS(-Wall,               DEF_CXXFLAGS)
AX_APPEND_COMPILE_FLAGS(-fstack-protector,   DEF_CXXFLAGS)
AX_APPEND_COMPILE_FLAGS(-fPIE,               DEF_CXXFLAGS)
AX_APPEND_COMPILE_FLAGS(-D_FORTIFY_SOURCE=2, DEF_CXXFLAGS)
AX_APPEND_LINK_FLAGS([-fPIE -pie],   DEF_LDFLAGS)
AX_APPEND_LINK_FLAGS([-Wl,-z,relro], DEF_LDFLAGS)
AX_APPEND_LINK_FLAGS([-Wl,-z,now],   DEF_LDFLAGS)
AC_LANG_POP([C++])

test "x$enable_CXXFLAGS_setting" = xyes && AC_SUBST(CXXFLAGS, $DEF_CXXFLAGS)
test "x$enable_LDFLAGS_setting"  = xyes && AC_SUBST(LDFLAGS,  $DEF_LDFLAGS)

# Checks for programs.
AC_PROG_CXX
AC_PROG_CC
AC_PROG_CXXCPP
AC_PROG_CPP

# Check for C++ gmpxx headers.
AC_LANG_PUSH([C++])
AC_CHECK_HEADERS([gmpxx.h])
AC_CHECK_LIB(gmpxx,abs,AC_SUBST(LIBGMPXX,'-lgmpxx -lgmp'),[
	AC_MSG_ERROR([GMPlib not found])],[-lgmp])
AX_LIB_CXX_REGEX
AC_LANG_POP([C++])

# check for Boost library
AC_MSG_CHECKING([for libboost regex support])
AX_BOOST_BASE([1.33.1],[],[AC_MSG_ERROR([Boost not found])])

AC_ARG_ENABLE(parsergeneration,AS_HELP_STRING([--disable-parsergeneration],
	      [toggle parser generation from grammar description (default: yes).
	       This requires bisonc++ and flexc++.]))

if test "x$enable_parsergeneration" != "xno"; then
	AC_SUBST(PARSERGEN_ENABLED,[yes])

	AC_PROG_EGREP
	AC_MSG_CHECKING([for bisonc++])
	AS_IF([bisonc++ --version | $EGREP -q '^bisonc\+\+ V'], [AC_MSG_RESULT([yes])], [AC_MSG_ERROR([bisonc++ not found (required for dist build)])])
	AC_MSG_CHECKING([for flex++])
	AS_IF([flexc++ --version | $EGREP -q '^flexc\+\+ V'], [AC_MSG_RESULT([yes])], [AC_MSG_ERROR([flexc++ not found (required for dist build)])])
else
	AC_MSG_NOTICE([parser generation disabled])
fi

# Check option to link statically against "special" libraries for
# submit client and checktestdata programs.
AC_ARG_ENABLE(static-linking,AS_HELP_STRING([--enable-static-linking],
[enable static linking against libgmpxx (default: no).]))

if test "x$enable_static_linking" = "xyes"; then
	AC_SUBST(STATIC_LINK_START,[-Wl,-Bstatic])
	AC_SUBST(STATIC_LINK_END,  [-Wl,-Bdynamic])
fi

AC_CONFIG_FILES([config.mk])
AC_OUTPUT

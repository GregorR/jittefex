#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ([2.60])
AC_INIT([jittefex], [0.1], [])
AC_CONFIG_SRCDIR([src/jit.cc])
AC_CONFIG_HEADERS([src/acconfig.h])
AC_CONFIG_AUX_DIR([scripts])

m4_include([scripts/lean.m4])

AM_INIT_AUTOMAKE

# Checks for programs.
AC_PROG_CC([clang gcc cc])
AC_PROG_CXX([clang++ g++ c++])
AC_PROG_RANLIB

# Checks for libraries.
AC_ARG_WITH([llvm],
    [AS_HELP_STRING([--with-llvm], [enable support for LLVM's ORC JIT])],
    [], [with_llvm=""])
#'
LLVM_CXXFLAGS=
LLVM_LDFLAGS=
LLVM_LIBS=
JITTEFEX_HAVE_LLVM='#undef JITTEFEX_HAVE_LLVM'
AS_IF([test x"$with_llvm" != xno], [
printf 'checking for LLVM... '
LLVM_CONFIG=llvm-config
if test x"$with_llvm" != xyes -a x"$with_llvm" != x
then
    LLVM_CONFIG="$with_llvm/bin/llvm-config"
fi
$[]LLVM_CONFIG --libs core orcjit native > /dev/null 2>&1
if test "$?" = "0"
then
    printf 'yes\n'
    AC_DEFINE([HAVE_LLVM], [1], [LLVM is available])
    LLVM_CXXFLAGS="`$[]LLVM_CONFIG --cxxflags`"
    LLVM_LDFLAGS="`$[]LLVM_CONFIG --ldflags`"
    LLVM_LIBS="`$[]LLVM_CONFIG --libs core orcjit native` `$[]LLVM_CONFIG --system-libs core orcjit native`"
    JITTEFEX_HAVE_LLVM='#define JITTEFEX_HAVE_LLVM 1'
else
    printf 'no\n'
    if test x"$with_llvm" != x
    then
        $[]LLVM_CONFIG --libs core orcjit native
        exit 1
    fi
fi
])
AC_SUBST([LLVM_CXXFLAGS])
AC_SUBST([LLVM_LDFLAGS])
AC_SUBST([LLVM_LIBS])
AC_SUBST([JITTEFEX_HAVE_LLVM])

AC_ARG_WITH([sfjit],
    [AS_HELP_STRING([--with-sfjit], [enable support for the SFJIT fast JIT])],
    [], [with_sfjit=""])
#'
JITTEFEX_HAVE_SFJIT='#undef JITTEFEX_HAVE_SFJIT'
AS_IF([test x"$with_sfjit" != xno], [
    printf 'checking for SFJIT support... '
    PRE_SFJIT_CFLAGS="${CFLAGS}"
    CFLAGS="$CFLAGS -I${srcdir}"
    AC_COMPILE_IFELSE(
        [AC_LANG_PROGRAM([[
#include "src/sfjit/sljitLir.h"
#if (defined SLJIT_CONFIG_UNSUPPORTED && SLJIT_CONFIG_UNSUPPORTED)
#error 1
#endif
            ]], [[]])],
        [
            AC_MSG_RESULT([yes])
            with_sfjit=yes
            JITTEFEX_HAVE_SFJIT='#define JITTEFEX_HAVE_SFJIT 1'
        ],
        [
            AC_MSG_RESULT([no])
            if test x"$with_sfjit" = xyes
            then
                exit 1
            fi
            with_sfjit=no
        ])
    CFLAGS="$PRE_SFJIT_CFLAGS"
])
AC_SUBST([JITTEFEX_HAVE_SFJIT])


# Checks for header files.

# Checks for typedefs, structures, and compiler characteristics.

# Checks for library functions.

# Checks for options.
JITTEFEX_ENABLE_DEBUG='#undef JITTEFEX_ENABLE_DEBUG'
AC_ARG_ENABLE([debug],
    [AS_HELP_STRING([--enable-debug], [enable debugging features and checks])],
    [], [enable_debug=no])
AS_IF([test x"$enable_debug" != xno], [
    JITTEFEX_ENABLE_DEBUG='#define JITTEFEX_ENABLE_DEBUG 1'
], [
    CFLAGS="$CFLAGS -DNDEBUG=1"
    CXXFLAGS="$CXXFLAGS -DNDEBUG=1"
])
AC_SUBST([JITTEFEX_ENABLE_DEBUG])

AC_CONFIG_FILES([Makefile src/Makefile examples/Makefile src/jittefex/config.h])
AC_OUTPUT

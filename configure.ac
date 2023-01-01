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
AC_PROG_CC
AC_PROG_CXX
AC_PROG_RANLIB

# Checks for libraries.
llvm-config --libs core orcjit native > /dev/null 2>&1
if test "$?" = "0"
then
    AC_DEFINE([HAVE_LLVM], [1], [LLVM is available])
    LLVM_CXXFLAGS="`llvm-config --cxxflags`"
    LLVM_LDFLAGS="`llvm-config --ldflags`"
    LLVM_LIBS="`llvm-config --libs core orcjit native` `llvm-config --system-libs core orcjit native`"
    JITTEFEX_HAVE_LLVM='#define JITTEFEX_HAVE_LLVM 1'
else
    LLVM_CXXFLAGS=
    LLVM_LDFLAGS=
    LLVM_LIBS=
    JITTEFEX_HAVE_LLVM='#undef JITTEFEX_HAVE_LLVM'
fi
AC_SUBST([LLVM_CXXFLAGS])
AC_SUBST([LLVM_LDFLAGS])
AC_SUBST([LLVM_LIBS])
AC_SUBST([JITTEFEX_HAVE_LLVM])

# Checks for header files.

# Checks for typedefs, structures, and compiler characteristics.

# Checks for library functions.

AC_CONFIG_FILES([Makefile src/Makefile src/jittefex/config.h])
AC_OUTPUT

AUTOMAKE_OPTIONS=foreign

noinst_PROGRAMS=jittefex-kaleidoscope jittefex-kaleidoscope-int

jittefex_kaleidoscope_SOURCES=toy.cc
jittefex_kaleidoscope_CXXFLAGS=@LLVM_CXXFLAGS@ \
	-I$(srcdir)/../src -I../src -I$(srcdir)/../src/jittefex/config-nogc
jittefex_kaleidoscope_LDFLAGS=@LLVM_LDFLAGS@
jittefex_kaleidoscope_LDADD=../src/libjittefex-nogc.a @LLVM_LIBS@

jittefex_kaleidoscope_int_SOURCES=toy-int.cc
jittefex_kaleidoscope_int_CXXFLAGS=@LLVM_CXXFLAGS@ \
	-I$(srcdir)/../src -I../src -I$(srcdir)/../src/jittefex/config-nogc
jittefex_kaleidoscope_int_LDFLAGS=@LLVM_LDFLAGS@
jittefex_kaleidoscope_int_LDADD=../src/libjittefex-nogc.a @LLVM_LIBS@

../src/libjittefex-nogc.a:
	cd ../src ; $(MAKE) libjittefex-nogc.a

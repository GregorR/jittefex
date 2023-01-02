AUTOMAKE_OPTIONS=foreign

bin_PROGRAMS=jittefex-kaleidoscope jittefex-kaleidoscope-int

jittefex_kaleidoscope_SOURCES=toy.cc
jittefex_kaleidoscope_CXXFLAGS=@LLVM_CXXFLAGS@ -std=c++17 -I${srcdir}/../src -I../src
jittefex_kaleidoscope_LDFLAGS=@LLVM_LDFLAGS@
jittefex_kaleidoscope_LDADD=../src/libjittefex.a @LLVM_LIBS@

jittefex_kaleidoscope_int_SOURCES=toy-int.cc
jittefex_kaleidoscope_int_CXXFLAGS=@LLVM_CXXFLAGS@ -std=c++17 -I${srcdir}/../src -I../src
jittefex_kaleidoscope_int_LDFLAGS=@LLVM_LDFLAGS@
jittefex_kaleidoscope_int_LDADD=../src/libjittefex.a @LLVM_LIBS@

../src/libjittefex.a:
	cd ../src ; $(MAKE) libjittefex.a

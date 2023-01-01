AUTOMAKE_OPTIONS=foreign

bin_PROGRAMS=jittefex-kaleidoscope

jittefex_kaleidoscope_SOURCES=toy.cc
jittefex_kaleidoscope_CXXFLAGS=@LLVM_CXXFLAGS@ -std=c++17 -I${srcdir}/../src -I../src
jittefex_kaleidoscope_LDFLAGS=@LLVM_LDFLAGS@
jittefex_kaleidoscope_LDADD=../src/libjittefex.a @LLVM_LIBS@

../src/libjittefex.a:
	cd ../src ; $(MAKE) libjittefex.a

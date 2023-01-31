AUTOMAKE_OPTIONS=foreign

lib_LIBRARIES=libjittefex.a libjittefex-nogc.a libjittefex-notag.a
bin_PROGRAMS=jittefex-config

LIBJITTEFEX_SRCS=\
	builder.cc compile.cc ir.cc jit.cc sfjitLir.cc type.cc

libjittefex_a_SOURCES=$(LIBJITTEFEX_SRCS) ggggc.cc
libjittefex_a_CXXFLAGS=@LLVM_CXXFLAGS@ -I$(srcdir)/jittefex/config-gc -I$(srcdir)/ggggc

libjittefex_notag_a_SOURCES=$(LIBJITTEFEX_SRCS) ggggc.cc
libjittefex_notag_a_CXXFLAGS=@LLVM_CXXFLAGS@ -I$(srcdir)/jittefex/config-notag -I$(srcdir)/ggggc

libjittefex_nogc_a_SOURCES=$(LIBJITTEFEX_SRCS)
libjittefex_nogc_a_CXXFLAGS=@LLVM_CXXFLAGS@ -I$(srcdir)/jittefex/config-nogc

jittefex_config_SOURCES=jittefex-config.cc
jittefex_config_CXXFLAGS=-I$(srcdir)/jittefex/config-gc

nobase_include_HEADERS=\
	jittefex/builder.h jittefex/compile.h jittefex/gc.h \
	jittefex/Instruction.def jittefex/instruction.h jittefex/ir.h \
	jittefex/jit.h jittefex/type.h \
	jittefex/config-gc/jittefex/config.h \
	jittefex/config-nogc/jittefex/config.h \
	jittefex/config-prime.h \
	jittefex/ggggc/gc-gembc.h jittefex/ggggc/gc.h \
	jittefex/ggggc/gc-portablems.h jittefex/ggggc/gen-barriers.h \
	jittefex/ggggc/push.h jittefex/ggggc/thread-locals.h \
	jittefex/ggggc/threads.h jittefex/ggggc/threads-macosx.h \
	jittefex/ggggc/threads-none.h jittefex/ggggc/threads-posix.h \
	jittefex/ggggc/threads-windows.h \
	jittefex/tl/optional.hpp
